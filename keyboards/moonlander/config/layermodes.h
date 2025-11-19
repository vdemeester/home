// Numword layer mode header

#pragma once

#include QMK_KEYBOARD_H

// Enable/disable numword mode with specific layer
void enable_num_word(uint8_t layer);
void disable_num_word(uint8_t layer);
bool is_num_word_enabled(void);

// Process numword activation (should be called from custom keycode handler)
void process_num_word_activation(uint8_t layer, const keyrecord_t *record);

// Process numword logic (should be called from process_record_user)
// Returns true to continue processing, false to stop
bool process_num_word(uint16_t keycode, const keyrecord_t *record);
