/*
 * Copyright (c) 2011-2012, Renaud Mariana <rmariana@gmail.com>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */
 
#include "erl_comm.h"

#define CHK(stmt, msg) if((stmt) < 0) {puts("ERROR: "#msg); exit(1);}
#define TICKS_PER_QUARTER 128
#define PORT        128

typedef struct {
  int queue_id, port_in_id, tempo, NticksPerMeasure;
  snd_seq_t *seq_handle;  
} state_t;



snd_seq_tick_time_t 
get_tick(state_t* st) {
  snd_seq_queue_status_t *status;
  snd_seq_tick_time_t current_tick;
  
  snd_seq_queue_status_malloc(&status);
  snd_seq_get_queue_status(st->seq_handle, st->queue_id, status);
  current_tick = snd_seq_queue_status_get_tick_time(status);
  snd_seq_queue_status_free(status);
  return(current_tick);
}

static void 
sigterm_exit(state_t *st, int sig);

static void 
open_seq(state_t* st) {

  if (snd_seq_open(&st->seq_handle, "default", SND_SEQ_OPEN_DUPLEX, 0) < 0) {
    fprintf(stderr, "Error opening ALSA sequencer.\n");
    exit(1);
  }

  if ((st->port_in_id = 
       snd_seq_create_simple_port(st->seq_handle, "Ursus-80",
				  SND_SEQ_PORT_CAP_WRITE|SND_SEQ_PORT_CAP_SUBS_WRITE,
				  SND_SEQ_PORT_TYPE_APPLICATION)) < 0) {
    fprintf(stderr, "Error creating sequencer port.\n");
    exit(1);
  }
}

static void 
init_queue(state_t* st) {
  int seq_len = 4;   

  st->queue_id = snd_seq_alloc_queue(st->seq_handle);
  snd_seq_set_client_pool_output(st->seq_handle, (seq_len<<1) + 4);
}

static void clear_queue(state_t* st) {
  snd_seq_remove_events_t *remove_ev;

  snd_seq_remove_events_malloc(&remove_ev);
  snd_seq_remove_events_set_queue(remove_ev, st->queue_id);
  snd_seq_remove_events_set_condition(remove_ev, SND_SEQ_REMOVE_OUTPUT | SND_SEQ_REMOVE_IGNORE_OFF);
  snd_seq_remove_events(st->seq_handle, remove_ev);
  snd_seq_remove_events_free(remove_ev);
}


void set_tempo(state_t* st, int bpm) {
  snd_seq_queue_tempo_t *queue_tempo;

  snd_seq_queue_tempo_malloc(&queue_tempo);
  st->tempo = (int)(6e7 / ((double)bpm * (double)TICKS_PER_QUARTER) * (double)TICKS_PER_QUARTER);
  snd_seq_queue_tempo_set_tempo(queue_tempo, st->tempo);
  snd_seq_queue_tempo_set_ppq(queue_tempo, TICKS_PER_QUARTER);
  snd_seq_set_queue_tempo(st->seq_handle, st->queue_id, queue_tempo);
  snd_seq_queue_tempo_free(queue_tempo);
}


static void 
start_seq_echo(state_t* st, int tick) {
  snd_seq_event_t ev;

  snd_seq_ev_clear(&ev);
  ev.type = SND_SEQ_EVENT_ECHO; 
  snd_seq_ev_schedule_tick(&ev, st->queue_id,  0, tick);
  snd_seq_ev_set_dest(&ev, snd_seq_client_id(st->seq_handle), st->port_in_id);
  snd_seq_event_output_direct(st->seq_handle, &ev);
}


static void 
notify_erlang_midi_event_type(int type, int tick) {

  if (tick != 0) { 
    
    byte buff[]= {0, SND_SEQ_EVENT_ECHO, 0,0,0,0};

    write_int(tick, buff+2);
    write_cmd(buff, 6);
  }
}


static int 
erlang_cmd(state_t* st, snd_seq_tick_time_t tick) {
  int  result= -1;
  byte buff[8];

  if (read_cmd(buff) <0)
    return -1;
 
  switch  ( buff[0]) {
  case 3 :
    /* fprintf(stderr,"start seq %i %i\r\n",buff[1], buff[2]); */
    set_tempo(st, buff[1] );
    st->NticksPerMeasure = buff[2];
    start_seq_echo(st, tick);
    result = 0;
    break;
  case 4 :
    result = -1;
    break;
  default :
    fprintf(stderr,"*** UNIDENTIFIED erlang_cmd: %i\r\n",buff[0]);

  }

  buff[0] = result;
  write_cmd(buff, 1);
  return result;
}


static snd_seq_tick_time_t 
midi_action(state_t* st, snd_seq_tick_time_t tick) {
  do {
    snd_seq_event_t *ev;

    snd_seq_event_input(st->seq_handle, &ev);
    switch (ev->type) {
    case SND_SEQ_EVENT_ECHO:
      notify_erlang_midi_event_type(ev->type, tick);
      tick += st->NticksPerMeasure;  
      start_seq_echo(st, tick);
      break;

    default:
      fprintf(stderr, "*** UNIDENTIFIED  MIDI EVENT: %d \n", ev->type);

    }
    snd_seq_free_event(ev);
  } while (snd_seq_event_input_pending(st->seq_handle, 0) > 0);
  return tick; 
}


static void 
sigterm_exit(state_t* st, int sig) {
  /* fprintf(stderr, "\n\nsigterm_exit\n"); */
  clear_queue(st);
  sleep(2);
  snd_seq_stop_queue(st->seq_handle, st->queue_id, NULL);
  snd_seq_free_queue(st->seq_handle, st->queue_id);
  exit(0);
}


int 
main(int argc, char *argv[]) {

  snd_seq_tick_time_t tick;
  int npfd =0;
  struct pollfd *pfd;
  state_t st;

  open_seq(&st);

  init_queue(&st);
  set_tempo(&st, 10);
  snd_seq_start_queue(st.seq_handle, st.queue_id, NULL);
  snd_seq_drain_output(st.seq_handle);
  npfd = snd_seq_poll_descriptors_count(st.seq_handle, POLLIN)+1;
  pfd = (struct pollfd *)malloc(npfd * sizeof(struct pollfd));
  snd_seq_poll_descriptors(st.seq_handle, pfd, npfd-1, POLLIN);
  pfd[npfd-1].fd = 0;
  pfd[npfd-1].events = POLLIN;
  tick = 0;

  while (1) {
    if (poll(pfd, npfd, 100000) > 0) {
      int l1 = 0;
      for (; l1 < npfd-1; l1++) {
	if (pfd[l1].revents > 0) {
	  tick = midi_action(&st, tick); 
	}
      }
      if (pfd[npfd-1].revents > 0) {
	if (erlang_cmd(&st, tick) < 0) {
	  break;
	}
      }  
    }

  }
  sigterm_exit(&st, 0);
  return 0;
}
