/* hwclock.c */
#include <erl_nif.h>
#include <stdio.h>
#include <signal.h>
#include <alsa/asoundlib.h>
#include <sys/types.h>
#include <unistd.h>

#define exit_on_fail(result, msg) { \
    int error = result; \
    if (error < 0) { \
        printf("ERROR: %s (%s)\n", msg, snd_strerror(error)); \
        exit(0); \
    } \
}

typedef struct {
    int queue_id, port_in_id, tempo, NticksPerMeasure;
    snd_seq_t *seq_handle;
    struct pollfd *pfd;
    int npfd;
    snd_seq_tick_time_t tick;
} state_t;

static ErlNifResourceType *type_hwclock;
static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_undefined;
static ERL_NIF_TERM atom_null;

snd_seq_tick_time_t get_tick(state_t *st);

void set_tempo(state_t *st, int bpm, int ppq);

snd_seq_tick_time_t get_tick(state_t *st) {
    snd_seq_queue_status_t *status;
    snd_seq_tick_time_t current_tick;

    snd_seq_queue_status_malloc(&status);
    snd_seq_get_queue_status(st->seq_handle, st->queue_id, status);
    current_tick = snd_seq_queue_status_get_tick_time(status);
    snd_seq_queue_status_free(status);
    return (current_tick);
}


static void open_seq(state_t *st) {

    exit_on_fail(snd_seq_open(&st->seq_handle, "default", SND_SEQ_OPEN_DUPLEX, 0), "snd_seq_open" );

    exit_on_fail(st->port_in_id = snd_seq_create_simple_port(
                                      st->seq_handle, "Ursus-80",
                                      SND_SEQ_PORT_CAP_WRITE | SND_SEQ_PORT_CAP_SUBS_WRITE,
                                      SND_SEQ_PORT_TYPE_APPLICATION), "snd_seq_create_simple_port");
}
static void close_seq(state_t *st) {

    exit_on_fail(snd_seq_delete_simple_port(st->seq_handle, st->port_in_id), "snd_seq_delete_simple_port" );
    exit_on_fail(snd_seq_close(st->seq_handle), "snd_seq_close" );
    st->seq_handle = NULL;
}

static void init_queue(state_t *st) {
    int seq_len = 4;

    st->queue_id = snd_seq_alloc_queue(st->seq_handle);
    snd_seq_set_client_pool_output(st->seq_handle, (seq_len << 1) + 4);
}

static void clear_queue(state_t *st) {
    snd_seq_remove_events_t *remove_ev;

    exit_on_fail(snd_seq_stop_queue(st->seq_handle, st->queue_id, NULL), "snd_seq_stop_queue");
    exit_on_fail(snd_seq_remove_events_malloc(&remove_ev), "snd_seq_remove_events_malloc");
    snd_seq_remove_events_set_queue(remove_ev, st->queue_id);
    snd_seq_remove_events_set_condition(remove_ev, SND_SEQ_REMOVE_OUTPUT |
                                        SND_SEQ_REMOVE_IGNORE_OFF);
    snd_seq_remove_events(st->seq_handle, remove_ev);
    snd_seq_remove_events_free(remove_ev);

    exit_on_fail(snd_seq_free_queue(st->seq_handle, st->queue_id), "snd_seq_free_queue");
}

void set_tempo(state_t *st, int bpm, int ppq) {
    snd_seq_queue_tempo_t *queue_tempo;

    snd_seq_queue_tempo_malloc(&queue_tempo);
    st->tempo = (int)(6e7 / (double)bpm );
    snd_seq_queue_tempo_set_tempo(queue_tempo, st->tempo);
    snd_seq_queue_tempo_set_ppq(queue_tempo, ppq);
    snd_seq_set_queue_tempo(st->seq_handle, st->queue_id, queue_tempo);
    snd_seq_queue_tempo_free(queue_tempo);
}

static void start_seq_echo(state_t *st, int tick) {
    snd_seq_event_t ev;

    snd_seq_ev_clear(&ev);
    ev.type = SND_SEQ_EVENT_ECHO;
    snd_seq_ev_schedule_tick(&ev, st->queue_id, 0, tick);
    snd_seq_ev_set_dest(&ev, snd_seq_client_id(st->seq_handle), st->port_in_id);
    snd_seq_event_output_direct(st->seq_handle, &ev);
}

static snd_seq_tick_time_t midi_action(state_t *st, snd_seq_tick_time_t tick) {
    do {
        snd_seq_event_t *ev;

        snd_seq_event_input(st->seq_handle, &ev);
        switch (ev->type) {
        case SND_SEQ_EVENT_ECHO:
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

/* NIF interface declarations */
static int load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info);

static ERL_NIF_TERM open_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM select_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM close_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ErlNifFunc nif_funcs[] =
{
    {"open", 3, open_nif},
    {"select", 2, select_nif},
    {"close", 1, close_nif}

};

ERL_NIF_INIT(hwclock, nif_funcs, load, NULL, NULL, NULL)

static int get_st_data(ErlNifEnv *env, ERL_NIF_TERM opaque, state_t **st) {
    return enif_get_resource(env, opaque, type_hwclock, (void **)st);
}


static void c_stop(ErlNifEnv* env, void* obj, ErlNifEvent event, int is_direct_call) {
    state_t *st = (state_t *)obj;

    if    (st->seq_handle == NULL) {
        return;
    }

    clear_queue(st);
    close_seq(st);

    free(st->pfd);
}

static int load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceTypeInit callbacks;

    callbacks.down      = NULL;
    callbacks.dtor      = NULL;
    callbacks.stop      = c_stop;
    callbacks.dyncall   = NULL;
    callbacks.members   = 4;

    type_hwclock = enif_init_resource_type(env, "ehw_clock", &callbacks,
                                           ERL_NIF_RT_CREATE, NULL);
    am_ok = enif_make_atom(env, "ok");
    am_error = enif_make_atom(env, "error");
    am_undefined = enif_make_atom(env, "undefined");
    atom_null = enif_make_atom(env, "null");

    *priv_data = NULL;

    return 0;
}

static ERL_NIF_TERM open_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    state_t *st;
    ERL_NIF_TERM nif_result;
    ErlNifUInt64 bpm, nticks, ppq;

    if (argc != 3 || !enif_get_uint64(env, argv[0], &bpm)
            || !enif_get_uint64(env, argv[1], &nticks)
            || !enif_get_uint64(env, argv[2], &ppq)
            || bpm > 255
            || nticks > 255
            || ppq > 1024) {
        return enif_make_badarg(env);
    }

    st = (state_t*)enif_alloc_resource(type_hwclock, sizeof(state_t));

    open_seq(st);

    init_queue(st);
    set_tempo(st, 10, ppq);
    exit_on_fail(snd_seq_start_queue(st->seq_handle, st->queue_id, NULL), "snd_seq_start_queue");
    exit_on_fail(snd_seq_drain_output(st->seq_handle), "snd_seq_drain_output");
    exit_on_fail(st->npfd = snd_seq_poll_descriptors_count(st->seq_handle, POLLIN), "snd_seq_poll_descriptors_count");
    st->pfd = (struct pollfd *)malloc(st->npfd * sizeof(struct pollfd));

    exit_on_fail(snd_seq_poll_descriptors(st->seq_handle, st->pfd, st->npfd, POLLIN), "snd_seq_poll_descriptors");

    st->tick = 0;
    set_tempo(st, bpm, ppq);
    st->NticksPerMeasure = nticks;
    start_seq_echo(st, st->tick);
    nif_result = enif_make_resource(env, st);

    enif_release_resource(st);

    return nif_result;
}

static ERL_NIF_TERM close_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    state_t *st;
    int retval;

    if (argc != 1  || !get_st_data(env, argv[0], &st)) {
        return enif_make_badarg(env);
    }

    if (st->seq_handle == NULL) {
        return am_ok;
    }

    retval = enif_select(env, st->pfd[0].fd, ERL_NIF_SELECT_STOP, st, NULL, am_undefined);
    if (retval < 0) {
        return enif_make_tuple2(env, am_error, enif_make_int(env, retval));
    }

    return am_ok;
}

static ERL_NIF_TERM select_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    state_t *st;
    ErlNifPid nifpid, *pid = NULL;
    int retval;

    if (argc != 2  || !get_st_data(env, argv[0], &st)) {
        return enif_make_badarg(env);
    }

    if (st->seq_handle == NULL) {
        return enif_make_tuple2(env, am_error, enif_make_int(env, -1));
    }

    if (argv[1] != atom_null) {
        if (!enif_get_local_pid(env, argv[1], &nifpid))
            return enif_make_badarg(env);
        pid = &nifpid;
    }

    retval = enif_select(env, st->pfd[0].fd, ERL_NIF_SELECT_READ, st, pid, enif_make_int(env, st->tick));
    if (retval < 0) {
        return enif_make_tuple2(env, am_error, enif_make_int(env, retval));
    }

    st->tick = midi_action(st, st->tick);

    return am_ok;
}
