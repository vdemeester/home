/* Copyright 2020 ZSA Technology Labs, Inc <@zsa>
 * Copyright 2020 Jack Humbert <jack.humb@gmail.com>
 * Copyright 2020 Christopher Courtney, aka Drashna Jael're  (@drashna) <drashna@live.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include QMK_KEYBOARD_H
#include "keymap_us.h"
#include "keymap_us_international_linux.h"
#include "layers.h"

const key_override_t circ_exclamation_override = ko_make_with_layers(MOD_MASK_SHIFT, KC_CIRC, KC_EXLM, 1 << BEPO);
const key_override_t dot_colon_override = ko_make_with_layers(MOD_MASK_SHIFT, KC_DOT, KC_COLN, 1 << BEPO);
const key_override_t comma_semicolon_override = ko_make_with_layers(MOD_MASK_SHIFT, HM_HYP_COMM, KC_SCLN, 1 << BEPO);

// bépo layer
// row 0:
// const key_override_t dquo_one_override = ko_make_with_layers(MOD_MASK_SHIFT, FR_DQUO, KC_1, 1 << BEPO); XXX: managed by a macro
const key_override_t ldaq_two_override = ko_make_with_layers(MOD_MASK_SHIFT, US_LDAQ, KC_2, 1 << BEPO);
const key_override_t rdaq_three_override = ko_make_with_layers(MOD_MASK_SHIFT, US_RDAQ, KC_3, 1 << BEPO);
const key_override_t lprn_four_override = ko_make_with_layers(MOD_MASK_SHIFT, KC_LPRN, KC_4, 1 << BEPO);
const key_override_t rprn_five_override = ko_make_with_layers(MOD_MASK_SHIFT, KC_RPRN, KC_5, 1 << BEPO);
const key_override_t at_six_override = ko_make_with_layers(MOD_MASK_SHIFT, KC_AT, KC_6, 1 << BEPO);
const key_override_t plus_seven_override = ko_make_with_layers(MOD_MASK_SHIFT, KC_PLUS, KC_7, 1 << BEPO);
const key_override_t minus_eight_override = ko_make_with_layers(MOD_MASK_SHIFT, KC_PMNS, KC_8, 1 << BEPO);
const key_override_t slash_nine_override = ko_make_with_layers(MOD_MASK_SHIFT, KC_PSLS, KC_9, 1 << BEPO);
const key_override_t star_zero_override = ko_make_with_layers(MOD_MASK_SHIFT, KC_PAST, KC_0, 1 << BEPO);

// row 1
const key_override_t b_pipe_override = ko_make_with_layers_and_negmods(MOD_BIT_RALT, KC_B, KC_PIPE, 1 << BEPO, MOD_MASK_SHIFT);
const key_override_t b_brkp_override = ko_make_with_layers(MOD_BIT_LSHIFT | MOD_BIT_RALT, KC_B, US_BRKP, 1 << BEPO);

const key_override_t *key_overrides[] = {
  &circ_exclamation_override,
  &dot_colon_override,
  // &quote_question_override,
  &comma_semicolon_override,

  // bépo
  &ldaq_two_override,
  &rdaq_three_override,
  &lprn_four_override,
  &rprn_five_override,
  &at_six_override,
  &plus_seven_override,
  &minus_eight_override,
  &slash_nine_override,
  &star_zero_override,
  &b_pipe_override,
  &b_brkp_override,
};
