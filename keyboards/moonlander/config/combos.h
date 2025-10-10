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

#pragma once

#define COMBO_REF_DEFAULT QWER

uint8_t combo_ref_from_layer(uint8_t layer){
  switch (get_highest_layer(layer_state)){
  case BEPO: return QWER;
  default: return QWER;
  }
  return layer;  // important if default is not in case.
}

enum combos {
  TO_BEPO, TO_ERGOL, TO_QWERTY,
  TOGGLE_MOUSE,
  BEPO_ESC, QWERTY_ESC,
};

const uint16_t PROGMEM combo_to_bepo[] = {LT(NAVI,KC_BSPC), KC_LSFT, COMBO_END};
const uint16_t PROGMEM combo_to_ergol[] = {LT(NUMB,KC_SPC), LT(SYMB, KC_ENT), COMBO_END};
const uint16_t PROGMEM combo_to_qwerty[] = {KC_DEL, KC_RALT, COMBO_END};
const uint16_t PROGMEM combo_toggle_mouse[] = {KC_Q, KC_R, COMBO_END};

const uint16_t PROGMEM combo_bepo_escape[] = {HM_ALT_R, HM_GUI_N, COMBO_END};
const uint16_t PROGMEM combo_qwe_escape[] = {HM_ALT_L, HM_GUI_SCLN, COMBO_END};

combo_t key_combos[] = {
  // Layers
  [TO_BEPO] = COMBO(combo_to_bepo, TO(BEPO)),
  [TO_ERGOL] = COMBO(combo_to_ergol, TO(ERGL)),
  [TO_QWERTY] = COMBO(combo_to_qwerty, TO(QWER)),
  [TOGGLE_MOUSE] = COMBO(combo_toggle_mouse, TG(MOUS)),
  // Others
  [QWERTY_ESC] = COMBO(combo_qwe_escape, KC_ESC),
  [BEPO_ESC] = COMBO(combo_bepo_escape, KC_ESC),
};

bool combo_should_trigger(uint16_t combo_index, combo_t *combo, uint16_t keycode, keyrecord_t *record) {
  switch (combo_index) {
  case BEPO_ESC:
    if (layer_state_is(BEPO)) {
      return true;
    }
    return false;
  case QWERTY_ESC:
    if (layer_state_is(QWER)) {
      return true;
    }
    return false;
  }
  return true;
}
