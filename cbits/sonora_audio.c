/*
 * Sonora audio output shim. See sonora_audio.h for the contract.
 *
 * The transport semantics here (end-of-buffer, looping, seek) mirror the
 * pure model in Sonora.Audio.Backend.advanceVoice; keep the two in sync.
 */
#define MA_NO_DECODING
#define MA_NO_ENCODING
#define MA_NO_GENERATION
#define MA_NO_RESOURCE_MANAGER
#define MA_NO_NODE_GRAPH
#define MA_NO_ENGINE
#define MINIAUDIO_IMPLEMENTATION
#include "miniaudio/miniaudio.h"

#include "sonora_audio.h"

#include <math.h>
#include <stdatomic.h>
#include <stdlib.h>
#include <string.h>

#if defined(_WIN32)
#include <windows.h>
static void sonora_yield(void) { SwitchToThread(); }
#else
#include <sched.h>
static void sonora_yield(void) { sched_yield(); }
#endif

#define SONORA_CHANNELS 2
/* Gain changes are smoothed over roughly this many seconds to avoid clicks. */
#define SONORA_GAIN_SMOOTHING_SECONDS 0.005

typedef struct sonora_voice {
    atomic_flag lock;
    float*   frames;
    uint64_t length;
    uint64_t position;
    uint64_t loop_start;
    uint64_t loop_end;
    int32_t  loop_enabled;
    int32_t  playing;
    uint32_t end_count;
    float    gain;          /* target */
    float    current_gain;  /* smoothed, owned by the callback */
} sonora_voice;

struct sonora_audio {
    ma_device     device;
    sonora_voice  voices[SONORA_VOICE_COUNT];
    _Atomic float master_gain;
    float         current_master_gain; /* owned by the callback */
    float         smoothing;           /* per-frame one-pole coefficient */
    atomic_int    closing;
    atomic_int    device_lost;
};

static void voice_lock(sonora_voice* v)
{
    while (atomic_flag_test_and_set_explicit(&v->lock, memory_order_acquire)) {
        sonora_yield();
    }
}

static int voice_try_lock(sonora_voice* v)
{
    return !atomic_flag_test_and_set_explicit(&v->lock, memory_order_acquire);
}

static void voice_unlock(sonora_voice* v)
{
    atomic_flag_clear_explicit(&v->lock, memory_order_release);
}

static int voice_loop_active(const sonora_voice* v)
{
    return v->loop_enabled && v->loop_end > v->loop_start && v->loop_end <= v->length;
}

static void mix_voice(sonora_voice* v, float* out, ma_uint32 frame_count, float smoothing)
{
    if (!v->playing || v->frames == NULL) {
        return;
    }
    const int looping = voice_loop_active(v);
    uint64_t pos = v->position;
    float g = v->current_gain;
    const float target = v->gain;

    for (ma_uint32 i = 0; i < frame_count; i += 1) {
        if (pos >= v->length) {
            v->playing = 0;
            v->end_count += 1;
            break;
        }
        g += (target - g) * smoothing;
        out[i * 2 + 0] += v->frames[pos * 2 + 0] * g;
        out[i * 2 + 1] += v->frames[pos * 2 + 1] * g;
        pos += 1;
        if (looping && pos == v->loop_end) {
            pos = v->loop_start;
        }
    }
    /* Reaching the end exactly on the last frame of this period. */
    if (v->playing && pos >= v->length) {
        v->playing = 0;
        v->end_count += 1;
    }
    v->position = pos;
    v->current_gain = g;
}

static void data_callback(ma_device* device, void* output, const void* input, ma_uint32 frame_count)
{
    (void)input;
    sonora_audio* audio = (sonora_audio*)device->pUserData;
    float* out = (float*)output; /* miniaudio pre-silences the output buffer */

    for (int i = 0; i < SONORA_VOICE_COUNT; i += 1) {
        sonora_voice* v = &audio->voices[i];
        /* Never block the audio thread; a contended voice is silent for one period. */
        if (voice_try_lock(v)) {
            mix_voice(v, out, frame_count, audio->smoothing);
            voice_unlock(v);
        }
    }

    const float target = atomic_load_explicit(&audio->master_gain, memory_order_relaxed);
    float mg = audio->current_master_gain;
    for (ma_uint32 i = 0; i < frame_count; i += 1) {
        mg += (target - mg) * audio->smoothing;
        for (int c = 0; c < SONORA_CHANNELS; c += 1) {
            float s = out[i * 2 + c] * mg;
            out[i * 2 + c] = s > 1.0f ? 1.0f : (s < -1.0f ? -1.0f : s);
        }
    }
    audio->current_master_gain = mg;
}

static void notification_callback(const ma_device_notification* notification)
{
    sonora_audio* audio = (sonora_audio*)notification->pDevice->pUserData;
    if (notification->type == ma_device_notification_type_stopped
        && !atomic_load(&audio->closing)) {
        atomic_store(&audio->device_lost, 1);
    }
}

static int valid_voice(int32_t voice)
{
    return voice >= 0 && voice < SONORA_VOICE_COUNT;
}

sonora_audio* sonora_audio_open(uint32_t sample_rate, int32_t* err_out)
{
    *err_out = MA_SUCCESS;
    sonora_audio* audio = (sonora_audio*)calloc(1, sizeof(sonora_audio));
    if (audio == NULL) {
        *err_out = MA_OUT_OF_MEMORY;
        return NULL;
    }
    for (int i = 0; i < SONORA_VOICE_COUNT; i += 1) {
        atomic_flag_clear(&audio->voices[i].lock);
        audio->voices[i].gain = 1.0f;
        audio->voices[i].current_gain = 1.0f;
    }
    atomic_init(&audio->master_gain, 1.0f);
    audio->current_master_gain = 1.0f;
    atomic_init(&audio->closing, 0);
    atomic_init(&audio->device_lost, 0);

    ma_device_config config = ma_device_config_init(ma_device_type_playback);
    config.playback.format   = ma_format_f32;
    config.playback.channels = SONORA_CHANNELS;
    config.sampleRate        = sample_rate; /* miniaudio resamples if the device differs */
    config.dataCallback      = data_callback;
    config.notificationCallback = notification_callback;
    config.pUserData         = audio;

    ma_result result = ma_device_init(NULL, &config, &audio->device);
    if (result != MA_SUCCESS) {
        free(audio);
        *err_out = (int32_t)result;
        return NULL;
    }
    audio->smoothing = (float)(1.0 - exp(-1.0 / (SONORA_GAIN_SMOOTHING_SECONDS * (double)audio->device.sampleRate)));

    result = ma_device_start(&audio->device);
    if (result != MA_SUCCESS) {
        atomic_store(&audio->closing, 1);
        ma_device_uninit(&audio->device);
        free(audio);
        *err_out = (int32_t)result;
        return NULL;
    }
    return audio;
}

void sonora_audio_close(sonora_audio* audio)
{
    if (audio == NULL) {
        return;
    }
    atomic_store(&audio->closing, 1);
    ma_device_uninit(&audio->device); /* joins the audio thread */
    for (int i = 0; i < SONORA_VOICE_COUNT; i += 1) {
        free(audio->voices[i].frames);
    }
    free(audio);
}

const char* sonora_audio_error_string(int32_t code)
{
    return ma_result_description((ma_result)code);
}

void sonora_audio_device_name(sonora_audio* audio, char* buf, uint32_t buf_len)
{
    if (buf_len == 0) {
        return;
    }
    buf[0] = '\0';
    size_t written = 0;
    ma_device_get_name(&audio->device, ma_device_type_playback, buf, buf_len, &written);
    buf[buf_len - 1] = '\0';
}

uint32_t sonora_audio_sample_rate(sonora_audio* audio)
{
    return audio->device.sampleRate;
}

float* sonora_audio_alloc_frames(uint64_t frame_count)
{
    if (frame_count == 0) {
        return NULL;
    }
    return (float*)malloc((size_t)(frame_count * SONORA_CHANNELS * sizeof(float)));
}

void sonora_audio_free_frames(float* frames)
{
    free(frames);
}

int32_t sonora_audio_load(sonora_audio* audio, int32_t voice, float* frames, uint64_t frame_count)
{
    if (!valid_voice(voice)) {
        free(frames);
        return MA_INVALID_ARGS;
    }
    sonora_voice* v = &audio->voices[voice];
    voice_lock(v);
    float* old = v->frames;
    v->frames = frames;
    v->length = frames == NULL ? 0 : frame_count;
    v->position = 0;
    v->playing = 0;
    v->loop_enabled = 0;
    v->loop_start = 0;
    v->loop_end = 0;
    v->current_gain = v->gain;
    voice_unlock(v);
    free(old); /* outside the lock: freeing a large buffer can be slow */
    return MA_SUCCESS;
}

void sonora_audio_set_playing(sonora_audio* audio, int32_t voice, int32_t playing)
{
    if (!valid_voice(voice)) {
        return;
    }
    sonora_voice* v = &audio->voices[voice];
    voice_lock(v);
    v->playing = (playing && v->frames != NULL && v->position < v->length) ? 1 : 0;
    voice_unlock(v);
}

void sonora_audio_seek(sonora_audio* audio, int32_t voice, uint64_t frame)
{
    if (!valid_voice(voice)) {
        return;
    }
    sonora_voice* v = &audio->voices[voice];
    voice_lock(v);
    v->position = frame > v->length ? v->length : frame;
    voice_unlock(v);
}

void sonora_audio_set_loop(sonora_audio* audio, int32_t voice, int32_t enabled, uint64_t start, uint64_t end)
{
    if (!valid_voice(voice)) {
        return;
    }
    sonora_voice* v = &audio->voices[voice];
    voice_lock(v);
    v->loop_enabled = enabled ? 1 : 0;
    v->loop_start = start;
    v->loop_end = end;
    voice_unlock(v);
}

void sonora_audio_set_gain(sonora_audio* audio, int32_t voice, float gain)
{
    if (!valid_voice(voice)) {
        return;
    }
    sonora_voice* v = &audio->voices[voice];
    voice_lock(v);
    v->gain = gain < 0.0f ? 0.0f : gain;
    voice_unlock(v);
}

void sonora_audio_set_master_gain(sonora_audio* audio, float gain)
{
    atomic_store_explicit(&audio->master_gain, gain < 0.0f ? 0.0f : gain, memory_order_relaxed);
}

void sonora_audio_status(sonora_audio* audio, int32_t voice, sonora_voice_status* out)
{
    memset(out, 0, sizeof(*out));
    if (!valid_voice(voice)) {
        return;
    }
    sonora_voice* v = &audio->voices[voice];
    voice_lock(v);
    out->position = v->position;
    out->length = v->length;
    out->end_count = v->end_count;
    out->playing = v->playing;
    voice_unlock(v);
}

int32_t sonora_audio_device_lost(sonora_audio* audio)
{
    return atomic_load(&audio->device_lost);
}

void sonora_audio_simulate(uint64_t length, uint64_t position, int32_t playing,
                           int32_t loop_enabled, uint64_t loop_start, uint64_t loop_end,
                           const uint32_t* chunks, uint32_t chunk_count,
                           sonora_voice_status* out)
{
    memset(out, 0, sizeof(*out));
    uint32_t max_chunk = 0;
    for (uint32_t i = 0; i < chunk_count; i += 1) {
        if (chunks[i] > max_chunk) {
            max_chunk = chunks[i];
        }
    }
    sonora_voice v;
    memset(&v, 0, sizeof(v));
    v.frames = length == 0 ? NULL : (float*)calloc((size_t)(length * SONORA_CHANNELS), sizeof(float));
    float* scratch = (float*)calloc((size_t)max_chunk * SONORA_CHANNELS + SONORA_CHANNELS, sizeof(float));
    if ((length > 0 && v.frames == NULL) || scratch == NULL) {
        free(v.frames);
        free(scratch);
        return;
    }
    v.length = length;
    v.position = position > length ? length : position;
    v.playing = (playing && v.frames != NULL && v.position < v.length) ? 1 : 0;
    v.loop_enabled = loop_enabled;
    v.loop_start = loop_start;
    v.loop_end = loop_end;
    v.gain = 1.0f;
    v.current_gain = 1.0f;
    for (uint32_t i = 0; i < chunk_count; i += 1) {
        mix_voice(&v, scratch, chunks[i], 1.0f);
    }
    out->position = v.position;
    out->length = v.length;
    out->end_count = v.end_count;
    out->playing = v.playing;
    free(v.frames);
    free(scratch);
}
