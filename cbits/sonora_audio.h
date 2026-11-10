/*
 * Sonora audio output shim.
 *
 * A small playback device built on miniaudio. It owns one output device and
 * two independent voices (song playback and patch audition) that are summed
 * into the device stream. The realtime callback only reads prepared
 * interleaved stereo float buffers; all musical decisions are made by the
 * Haskell caller.
 *
 * Thread model: every function except the device callback is called from
 * Haskell control threads. Voice state is guarded by a per-voice try-lock;
 * if the callback cannot take the lock it outputs silence for that period
 * rather than blocking the audio thread.
 */
#ifndef SONORA_AUDIO_H
#define SONORA_AUDIO_H

#include <stdint.h>

#define SONORA_VOICE_SONG     0
#define SONORA_VOICE_AUDITION 1
#define SONORA_VOICE_COUNT    2

typedef struct sonora_audio sonora_audio;

typedef struct sonora_voice_status {
    uint64_t position;    /* next frame to be played */
    uint64_t length;      /* frames in the loaded buffer */
    uint32_t end_count;   /* times the voice reached the end of its buffer */
    int32_t  playing;     /* 1 while advancing */
} sonora_voice_status;

/* Opens and starts the default playback device (stereo, float32).
 * Returns NULL on failure and writes a miniaudio result code to err_out. */
sonora_audio* sonora_audio_open(uint32_t sample_rate, int32_t* err_out);
void          sonora_audio_close(sonora_audio* audio);

/* Human-readable description of a miniaudio result code. */
const char*   sonora_audio_error_string(int32_t code);

/* Name of the opened device, written into buf (always NUL-terminated). */
void          sonora_audio_device_name(sonora_audio* audio, char* buf, uint32_t buf_len);
uint32_t      sonora_audio_sample_rate(sonora_audio* audio);

/* Buffer ownership: allocate with sonora_audio_alloc_frames, fill with
 * interleaved stereo samples, then hand to sonora_audio_load which takes
 * ownership (also on failure). Loading stops the voice and rewinds it. */
float*        sonora_audio_alloc_frames(uint64_t frame_count);
void          sonora_audio_free_frames(float* frames);
int32_t       sonora_audio_load(sonora_audio* audio, int32_t voice, float* frames, uint64_t frame_count);

void          sonora_audio_set_playing(sonora_audio* audio, int32_t voice, int32_t playing);
void          sonora_audio_seek(sonora_audio* audio, int32_t voice, uint64_t frame);
/* Loop [start, end). Disabled when enabled == 0 or end <= start. */
void          sonora_audio_set_loop(sonora_audio* audio, int32_t voice, int32_t enabled, uint64_t start, uint64_t end);
void          sonora_audio_set_gain(sonora_audio* audio, int32_t voice, float gain);
void          sonora_audio_set_master_gain(sonora_audio* audio, float gain);
void          sonora_audio_status(sonora_audio* audio, int32_t voice, sonora_voice_status* out);

/* Non-zero once the device stopped without being asked to (unplugged,
 * driver reset). Playback must be reopened by the caller. */
int32_t       sonora_audio_device_lost(sonora_audio* audio);

/* Test hook: runs the realtime mixing routine over a silent buffer of the
 * given length, one call per chunk, without opening a device. Used to check
 * the native transport against the Haskell reference model. */
void          sonora_audio_simulate(uint64_t length, uint64_t position, int32_t playing,
                                    int32_t loop_enabled, uint64_t loop_start, uint64_t loop_end,
                                    const uint32_t* chunks, uint32_t chunk_count,
                                    sonora_voice_status* out);

#endif
