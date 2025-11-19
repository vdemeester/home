// Numword layer implementation
// Based on https://github.com/treeman/qmk_firmware/blob/master/keyboards/ferris/keymaps/treeman/layermodes.c
// and https://www.jonashietala.se/blog/2022/09/06/the_current_t-34_keyboard_layout/#Numbers

#include QMK_KEYBOARD_H
#include "layermodes.h"

static uint16_t num_word_timer;
static bool _num_word_enabled = false;
static uint8_t _num_word_layer = 0;

void enable_num_word(uint8_t layer) {
    _num_word_enabled = true;
    _num_word_layer = layer;
    layer_on(layer);
}

void disable_num_word(uint8_t layer) {
    _num_word_enabled = false;
    layer_off(layer);
}

bool is_num_word_enabled(void) {
    return _num_word_enabled;
}

void process_num_word_activation(uint8_t layer, const keyrecord_t *record) {
    if (record->event.pressed) {
        layer_on(layer);
        num_word_timer = timer_read();
    } else {
        if (timer_elapsed(num_word_timer) < TAPPING_TERM) {
            // Tapped, enable numword
            _num_word_enabled = true;
            _num_word_layer = layer;
        } else {
            // Held, just turn off the layer
            layer_off(layer);
        }
    }
}

// Returns true if numword should remain active after pressing this key
// Note: F-keys (F1-F15) are intentionally NOT in this list, so they will
// send from the NUMB layer and then automatically disable numword
static bool is_num_word_key(uint16_t keycode) {
    switch (keycode) {
        // Numbers
        case KC_1 ... KC_0:
        case KC_P1 ... KC_P0:
        // Numpad operators
        case KC_PAST:  // *
        case KC_PSLS:  // /
        case KC_PMNS:  // -
        case KC_PPLS:  // +
        case KC_PDOT:  // .
        case KC_PCMM:  // ,
        case KC_PEQL:  // =
        // Other operators and symbols commonly used with numbers
        case KC_PLUS:
        case KC_MINS:
        case KC_EQL:
        case KC_PERC:
        case KC_DOT:
        case KC_COMM:
        case KC_COLN:
        case KC_UNDS:
        // Special keys
        case KC_BSPC:
        case KC_DEL:
        case KC_ENT:
        case KC_SPC:
        case QK_REP:   // Repeat key
        case QK_AREP:  // Alternate repeat key
        // x for hexadecimal
        case KC_X:
        // Modifiers (so they don't disable numword)
        case KC_LSFT ... KC_RGUI:
        case OS_LSFT ... OS_RGUI:
            return true;
        default:
            return false;
    }
}

bool process_num_word(uint16_t keycode, const keyrecord_t *record) {
    if (!_num_word_enabled) {
        return true;
    }

    // Only check on key press to determine if we should disable
    if (record->event.pressed) {
        // Check if this key should keep numword active
        // If not, we'll disable on the RELEASE so the key sends from the layer first
        if (!is_num_word_key(keycode)) {
            // Mark for disable, but don't disable yet - let the key send first
        }
    } else {
        // On key release, check if we should disable numword
        if (!is_num_word_key(keycode)) {
            disable_num_word(_num_word_layer);
        }
    }

    return true;
}
