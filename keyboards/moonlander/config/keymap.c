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
#include "version.h"
#include "keymap_us.h"
#include "keymap_us_international_linux.h"

enum layers {
  BEPO,
  ERGL,
  QWER,
  SYMB,
  NUMB,
  NAVI,
  MOUS,
  MODS,
};

enum custom_keycodes {
  VRSN = SAFE_RANGE,
  FR_DQUO,
  FR_QUOT,
  FR_GRAVE,
  FR_CIRC,
  FR_E_AIGU,
  FR_E_AIGU_CAPS,
  FR_E_GRAVE,
  FR_E_GRAVE_CAPS,
  FR_A_GRAVE,
  FR_A_GRAVE_CAPS,
};

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

const key_override_t space_underscore_override = ko_make_with_layers_and_negmods(MOD_BIT_RALT, LT(NUMB, KC_SPC), KC_UNDS, 1 << BEPO, MOD_MASK_SHIFT);

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
  &space_underscore_override,
};

void leader_start_user(void) {
}

void leader_end_user(void) {
    if (leader_sequence_one_key(KC_F)) {
        // Leader, f => Types the below string
        SEND_STRING("QMK is awesome.");
    }
}

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
  // Experiments
  PIPE, AROBASE, HASH, DOLLAR, SLASH, BSLASH, AMPR, MINUS, UNDERSCORE,
  LEADER,
  LPRN, RPRN, LCBR, RCBR, LBRC, RBRC, LABK, RABK,
};

const uint16_t PROGMEM combo_to_bepo[] = {LT(NAVI,KC_BSPC), OS_LSFT, COMBO_END};
const uint16_t PROGMEM combo_to_ergol[] = {LT(NUMB,KC_SPC), LT(SYMB, KC_ENT), COMBO_END};
const uint16_t PROGMEM combo_to_qwerty[] = {KC_DEL, KC_RALT, COMBO_END};
const uint16_t PROGMEM combo_toggle_mouse[] = {KC_Q, KC_R, COMBO_END};

const uint16_t PROGMEM combo_bepo_escape[] = {HM_ALT_R, HM_GUI_N, COMBO_END};
const uint16_t PROGMEM combo_qwe_escape[] = {HM_ALT_L, HM_GUI_SCLN, COMBO_END};

const uint16_t PROGMEM combo_qwe_pipe[] = {KC_Q, KC_W, COMBO_END};       // |
const uint16_t PROGMEM combo_qwe_arobase[] = {KC_W, KC_E, COMBO_END};    // @
const uint16_t PROGMEM combo_qwe_hash[] = {KC_E, KC_R, COMBO_END};       // #
const uint16_t PROGMEM combo_qwe_ampr[] = {KC_R, KC_T, COMBO_END};       // &
const uint16_t PROGMEM combo_qwe_dollar[] = {KC_R, HM_CTL_F, COMBO_END}; // $
const uint16_t PROGMEM combo_qwe_slash[] = {KC_E, HM_SFT_D, COMBO_END};  // /
const uint16_t PROGMEM combo_qwe_bslash[] = {HM_SFT_D, KC_C, COMBO_END}; // antislash
const uint16_t PROGMEM combo_qwe_minus[] = {KC_W, HM_ALT_S, COMBO_END};  // -
const uint16_t PROGMEM combo_qwe_unds[] = {HM_CTL_F, KC_V, COMBO_END};   // _

const uint16_t PROGMEM combo_qwe_leader[] = {HM_SFT_D, HM_CTL_F, COMBO_END}; // FIXME: change this most likely.

const uint16_t PROGMEM combo_qwe_lprn[] = {KC_I, HM_SFT_K, COMBO_END};    // (
const uint16_t PROGMEM combo_qwe_rprn[] = {HM_SFT_K, KC_COMM, COMBO_END}; // )
const uint16_t PROGMEM combo_qwe_lcbr[] = {KC_U, HM_CTL_J, COMBO_END};    // {
const uint16_t PROGMEM combo_qwe_rcbr[] = {HM_CTL_J, KC_M, COMBO_END};    // }
const uint16_t PROGMEM combo_qwe_lbrc[] = {KC_O, HM_ALT_L, COMBO_END};    // [
const uint16_t PROGMEM combo_qwe_rbrc[] = {HM_ALT_L, KC_DOT, COMBO_END};  // ]
const uint16_t PROGMEM combo_qwe_labk[] = {KC_U, KC_I, COMBO_END};        // <
const uint16_t PROGMEM combo_qwe_rabk[] = {KC_I, KC_O, COMBO_END};        // >

combo_t key_combos[] = {
  // Layers
  [TO_BEPO] = COMBO(combo_to_bepo, TO(BEPO)),
  [TO_ERGOL] = COMBO(combo_to_ergol, TO(ERGL)),
  [TO_QWERTY] = COMBO(combo_to_qwerty, TO(QWER)),
  [TOGGLE_MOUSE] = COMBO(combo_toggle_mouse, TG(MOUS)),
  // Others
  [QWERTY_ESC] = COMBO(combo_qwe_escape, KC_ESC),
  [BEPO_ESC] = COMBO(combo_bepo_escape, KC_ESC),
  // Experiments
  [PIPE] = COMBO(combo_qwe_pipe, KC_PIPE),
  [AROBASE] = COMBO(combo_qwe_arobase, KC_AT),
  [HASH] = COMBO(combo_qwe_hash, KC_HASH),
  [DOLLAR] = COMBO(combo_qwe_dollar, KC_DLR),
  [SLASH] = COMBO(combo_qwe_slash, KC_SLSH),
  [BSLASH] = COMBO(combo_qwe_bslash, KC_BSLS),
  [AMPR] = COMBO(combo_qwe_ampr, KC_AMPR),
  [MINUS] = COMBO(combo_qwe_minus, KC_PMNS),
  [UNDERSCORE] = COMBO(combo_qwe_unds, KC_UNDS),

  [LEADER] = COMBO(combo_qwe_leader, QK_LEAD),
  
  [LPRN] = COMBO(combo_qwe_lprn, KC_LPRN),
  [RPRN] = COMBO(combo_qwe_rprn, KC_RPRN),
  [LCBR] = COMBO(combo_qwe_lcbr, KC_LCBR),
  [RCBR] = COMBO(combo_qwe_rcbr, KC_RCBR),
  [LBRC] = COMBO(combo_qwe_lbrc, KC_LBRC),
  [RBRC] = COMBO(combo_qwe_rbrc, KC_RBRC),
  [LABK] = COMBO(combo_qwe_labk, KC_LABK),
  [RABK] = COMBO(combo_qwe_rabk, KC_RABK),
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

tap_dance_action_t tap_dance_actions[] = {
};

const char chordal_hold_layout[MATRIX_ROWS][MATRIX_COLS] PROGMEM = LAYOUT(
  'L', 'L', 'L', 'L', 'L', 'L', 'L', 'R', 'R', 'R', 'R', 'R', 'R', 'R',
  'L', 'L', 'L', 'L', 'L', 'L', 'L', 'R', 'R', 'R', 'R', 'R', 'R', 'R',
  'L', 'L', 'L', 'L', 'L', 'L', 'L', 'R', 'R', 'R', 'R', 'R', 'R', 'R',
  'L', 'L', 'L', 'L', 'L', 'L',           'R', 'R', 'R', 'R', 'R', 'R',
  'L', 'L', 'L', 'L', 'L', 'L',           'R', 'R', 'R', 'R', 'R', 'R',
                 'L', 'L', 'L',           'R', 'R', 'R'
);

// clang-format off
const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
  [BEPO] = LAYOUT(
		  // $  " « » ( ) @ + - / * = %
		  // shift: # 1 2 3 4 5 6 7 8 9 0 ° `
		  // ralt : – — < > [ ] ^ ± − ÷ × ≠ ‰
		  // shift+ralt : ¶ „ “ ” ≤ ≥ _ ¬ ¼ ½ ¾ ′ ″
		  // FIXME: should I invert ?
		  KC_DLR,  FR_DQUO,    US_LDAQ,    US_RDAQ,  KC_LPRN,    KC_RPRN,    XXXXXXX,           XXXXXXX, KC_AT,    KC_PLUS,    KC_PMNS,    KC_PSLS,    KC_PAST,    KC_PERC,
		  KC_TAB,  KC_B,    FR_E_AIGU,    KC_P,    KC_O,    FR_E_GRAVE,    XXXXXXX,           XXXXXXX, KC_CIRC,    KC_V,    KC_D,    KC_L,    KC_J,    KC_Z,
		  LT(NUMB, KC_EQL),  HM_GUI_A,    HM_ALT_U,    HM_SFT_I,    HM_CTL_E,    HM_HYP_COMM,    XXXXXXX,           XXXXXXX, HM_HYP_C,    HM_CTL_T,    HM_SFT_S,    HM_ALT_R,    HM_GUI_N, LT(SYMB,KC_M),
		  KC_GRV,  FR_A_GRAVE,    KC_Y,    KC_X,    KC_DOT,    KC_K,                                FR_QUOT,    KC_Q,    KC_G, KC_H,  KC_F, KC_W,
		  XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX, LT(MODS, KC_DEL),  QK_REP,               QK_AREP,      KC_RALT, XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX,
		  LT(NUMB, KC_SPC),  LT(NAVI,KC_BSPC), XXXXXXX,           XXXXXXX,  OS_LSFT,  LT(SYMB, KC_ENT)
		  ),
  [ERGL] = LAYOUT(
		  KC_EQL,  KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    XXXXXXX,           XXXXXXX, KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_MINS,
		  KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    XXXXXXX,           XXXXXXX, KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC,
		  KC_EQL,  HM_GUI_A,    HM_ALT_S,    HM_SFT_D,    HM_CTL_F,    HM_HYP_G,    XXXXXXX,           XXXXXXX, HM_HYP_H,    HM_CTL_J,    HM_SFT_K,    HM_ALT_L,    HM_GUI_SCLN, KC_QUOT,
		  KC_GRV,  KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,                                KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH, KC_RBRC,
		  XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX, KC_DEL,  QK_REP,               QK_AREP,      KC_RALT, XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX,
		  LT(NUMB, KC_SPC),  LT(NAVI,KC_BSPC), XXXXXXX,           XXXXXXX,  OS_LSFT,  LT(SYMB, KC_ENT)
		  ),
  [QWER] = LAYOUT(
		  KC_EQL,  KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    XXXXXXX,           XXXXXXX, KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_MINS,
		  KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    XXXXXXX,           XXXXXXX, KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC,
		  KC_EQL,  HM_GUI_A,    HM_ALT_S,    HM_SFT_D,    HM_CTL_F,    HM_HYP_G,    XXXXXXX,           XXXXXXX, HM_HYP_H,    HM_CTL_J,    HM_SFT_K,    HM_ALT_L,    HM_GUI_SCLN, KC_QUOT,
		  KC_GRV,  KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,                                KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH, KC_RBRC,
		  XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX, KC_DEL,  QK_REP,               QK_AREP,      KC_RALT, XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX,
		  LT(NUMB, KC_SPC),  LT(NAVI,KC_BSPC), XXXXXXX,           XXXXXXX,  OS_LSFT,  LT(SYMB, KC_ENT)
		  ),

  [SYMB] = LAYOUT(
		  VRSN,    _______,   _______,   _______,   _______,   _______,   XXXXXXX,           XXXXXXX, _______,  _______,  _______,  _______, _______,  _______,
		  KC_BSLS, FR_GRAVE, KC_LABK, KC_RABK, KC_PMNS, KC_PIPE,  XXXXXXX,           XXXXXXX, KC_CIRC, KC_LCBR, KC_RCBR, KC_DLR,  FR_CIRC, XXXXXXX,
		  XXXXXXX, KC_EXLM,  KC_PAST, KC_PSLS, KC_EQL,  KC_AMPR,  XXXXXXX,           XXXXXXX, KC_HASH, KC_LPRN, KC_RPRN, KC_SCLN, FR_DQUO, XXXXXXX,
		  XXXXXXX, KC_TILD,  KC_LBRC, KC_RBRC, KC_PLUS, KC_PERC,                              KC_AT,   KC_COLN, KC_COMM, KC_DOT,  FR_QUOT, XXXXXXX,
		  XXXXXXX, XXXXXXX,  XXXXXXX, XXXXXXX, _______,           RM_VALU,           RM_TOGG,          _______, XXXXXXX, XXXXXXX, XXXXXXX,  XXXXXXX,
		  RM_HUED, RM_VALD, RM_HUEU, TOGGLE_LAYER_COLOR,_______, _______
		  ),
  [NUMB] = LAYOUT(
		  VRSN,    _______,   _______,   _______,   _______,   _______,   _______,           _______, _______,  _______,  _______,  _______, _______,  _______,
		  _______, KC_F1,   KC_F2,   KC_F3,   KC_F4,   KC_F5,   _______,           _______, _______, KC_7,    KC_8,    KC_9,    KC_PAST, KC_PSLS,
		  _______, KC_F6,   KC_F7,   KC_F8,   KC_F9,   KC_F10,  _______,           _______, _______, KC_4,    KC_5,    KC_6,    KC_PMNS, KC_PPLS,
		  _______, KC_F11,  KC_F12,  KC_F13,  KC_F14,  KC_F15,                              _______, KC_1,    KC_2,    KC_3,    KC_PDOT, KC_PCMM,
		  EE_CLR,  _______, _______, _______, _______,          _______,           _______,          KC_0, _______, _______,    _______, _______,
		  _______, _______, _______,           _______, _______, _______
		  ),
  [NAVI] = LAYOUT(
		  VRSN,    _______,   _______,   _______,   _______,   _______,   _______,           _______, _______,  _______,  _______,  _______, _______,  QK_BOOT,
		  _______, _______, KC_AT,   KC_LCBR, KC_RCBR, KC_PIPE,  _______,           _______, KC_PSCR,   KC_HOME,    KC_UP,    KC_END,    KC_PGUP, _______,
		  _______, _______, KC_MPRV, KC_MPLY, KC_MNXT, _______,  _______,           _______, _______, KC_LEFT,    KC_DOWN,    KC_RGHT,    KC_PGDN, _______,
		  _______, _______, _______, _______, _______, _______,                              KC_MUTE, _______, _______, _______, _______, _______,
		  EE_CLR,  _______, _______, _______, _______,          KC_VOLD,           KC_VOLU,           _______, _______, _______, _______,  _______,
		  _______, _______, _______,           _______, _______, _______
		  ),
  
  [MOUS] = LAYOUT(
		  LED_LEVEL,_______,_______, _______, _______, _______, _______,           _______, _______, _______, _______, _______, _______, QK_BOOT,
		  _______, _______, _______, _______,   _______, _______, _______,           _______, _______, _______, MS_UP, _______, _______, _______,
		  _______, MS_WHLL, MS_WHLU, MS_WHLD, MS_WHLR, _______, _______,           _______, _______, MS_LEFT, MS_DOWN, MS_RGHT, _______, _______,
		  _______, _______, _______, _______, _______, _______,                              _______, _______, _______, _______, _______, _______,
		  EE_CLR,  _______, _______, _______, _______,          KC_VOLD,           KC_VOLU,           _______, _______, _______, _______,  _______,
		  MS_BTN1, MS_BTN3, _______,           _______, MS_BTN3, MS_BTN2
		  ),
  [MODS] = LAYOUT(
		  LED_LEVEL,_______,_______, _______, _______, _______, _______,           _______, _______, _______, _______, _______, _______, QK_BOOT,
		  _______, _______, _______, _______,   _______, _______, _______,           _______, _______, _______, MS_UP, _______, _______, _______,
		  _______, OS_LGUI,  OS_LALT,  OS_LSFT, OS_LCTL, OS_HYPR, _______,           _______, _______, OS_LCTL, OS_LSFT, OS_LALT, OS_LGUI, _______,
		  _______, _______, _______, _______, _______, _______,                              _______, _______, _______, _______, _______, _______,
		  EE_CLR,  _______, _______, _______, _______,          KC_VOLD,           KC_VOLU,           _______, _______, _______, _______,  _______,
		  MS_BTN1, MS_BTN3, _______,           _______, MS_BTN3, MS_BTN2
		  ),
};

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  uint8_t current_layer = get_highest_layer(layer_state);
  uint8_t mod_state = get_mods();
  if (record->event.pressed) {
    switch (keycode) {
      // FR_DQUO on bépo layout
    case VRSN:
      SEND_STRING (QMK_KEYBOARD "/" QMK_KEYMAP " @ " QMK_VERSION);
      return false;
    case SFTLLCK:
      if (record->tap.count) {
	if (record->event.pressed) {
	  // Toggle the lock on the highest layer.
	  layer_lock_invert(get_highest_layer(layer_state));
	}
	return false;
      }
      break;
    case FR_DQUO:
      if (record->event.pressed) {
	// if layer is bepo, and shift is on, send ?
	if (IS_LAYER_ON_STATE(current_layer, BEPO) && (get_mods() & MOD_MASK_SHIFT)) {
	  del_mods(MOD_MASK_SHIFT);
	  SEND_STRING(SS_TAP(X_1));
	  set_mods(mod_state);
	} else {
	  SEND_STRING(FR_DQUO_M);
	}
      }
      break;
    case FR_QUOT:
      // if layer is bepo, and shift is on, send ?
      if (IS_LAYER_ON_STATE(current_layer, BEPO) && (get_mods() & MOD_MASK_SHIFT)) {
	del_mods(MOD_MASK_SHIFT);
	SEND_STRING(SS_LSFT(SS_TAP(X_SLASH)));
	set_mods(mod_state);
      } else {
	SEND_STRING(FR_QUOT_M);
      }
      break;
    case FR_GRAVE:
      if (record->event.pressed) {
	SEND_STRING(FR_GRAVE_M);
      }
      break;
    case FR_CIRC:
      if (record->event.pressed) {
	SEND_STRING(FR_CIRC_M);
      }
      break;
    case FR_E_AIGU:
      if (record->event.pressed) {
	if (get_mods() & MOD_MASK_SHIFT) {
	  // É
	  del_mods(MOD_MASK_SHIFT);
	  SEND_STRING(FR_E_AIGU_CAPS_M);
	  set_mods(mod_state);
	} else {
	  // é
	  SEND_STRING(FR_E_AIGU_M);
	}
      }
      break;
    case FR_E_AIGU_CAPS:
      if (record->event.pressed) {
	SEND_STRING(FR_E_AIGU_CAPS_M);
      }
      break;
    case FR_E_GRAVE:
      if (record->event.pressed) {
	if (get_mods() & MOD_MASK_SHIFT) {
	  // 
	  del_mods(MOD_MASK_SHIFT);
	  SEND_STRING(FR_E_GRAVE_CAPS_M);
	  set_mods(mod_state);
	} else {
	  // è
	  SEND_STRING(FR_E_GRAVE_M); 
	}
      }
      break;
    case FR_E_GRAVE_CAPS:
      if (record->event.pressed) {
	SEND_STRING(FR_E_GRAVE_CAPS_M);
      }
      break;
    case FR_A_GRAVE:
      if (record->event.pressed) {
	if (get_mods() & MOD_MASK_SHIFT) {
	  del_mods(MOD_MASK_SHIFT);
	  SEND_STRING(FR_A_GRAVE_CAPS_M);
	  set_mods(mod_state);
	} else {
	  // à
	  SEND_STRING(FR_A_GRAVE_M); 
	}
      }
      break;
    case FR_A_GRAVE_CAPS:
      if (record->event.pressed) {
	SEND_STRING(FR_A_GRAVE_CAPS_M);
      }
      break;
    }
  }
  return true;
}

extern rgb_config_t rgb_matrix_config;

// Line 2 home-row (7 12 17 22 [27] and [63] 58 53 48 43)
void my_set_rgb_matrix_color_homerow(uint8_t red, uint8_t green, uint8_t blue) {
  rgb_matrix_set_color(7, red, green, blue);
  rgb_matrix_set_color(12, red, green, blue);
  rgb_matrix_set_color(17, red, green, blue);
  rgb_matrix_set_color(22, red, green, blue);
    
  rgb_matrix_set_color(58, red, green, blue);
  rgb_matrix_set_color(53, red, green, blue);
  rgb_matrix_set_color(48, red, green, blue);
  rgb_matrix_set_color(43, red, green, blue);
}

void my_set_rgb_matrix_color_line(int line,  uint8_t red, uint8_t green, uint8_t blue) {
  if (line == 0 || line == 1 || line == 2) {
    // Left
    rgb_matrix_set_color(line, red, green, blue);
    rgb_matrix_set_color(line+5, red, green, blue);
    rgb_matrix_set_color(line+10, red, green, blue);
    rgb_matrix_set_color(line+15, red, green, blue);
    rgb_matrix_set_color(line+20, red, green, blue);
    rgb_matrix_set_color(line+25, red, green, blue);
    /* rgb_matrix_set_color(line+29, red, green, blue); */
    // Right
    /* rgb_matrix_set_color(line+65, red, green, blue); */
    rgb_matrix_set_color(line+61, red, green, blue);
    rgb_matrix_set_color(line+56, red, green, blue);
    rgb_matrix_set_color(line+51, red, green, blue);
    rgb_matrix_set_color(line+46, red, green, blue);
    rgb_matrix_set_color(line+41, red, green, blue);
    rgb_matrix_set_color(line+36, red, green, blue);
  } else if (line == 3) {
    // Left
    rgb_matrix_set_color(3, red, green, blue);
    rgb_matrix_set_color(8, red, green, blue);
    rgb_matrix_set_color(13, red, green, blue);
    rgb_matrix_set_color(18, red, green, blue);
    rgb_matrix_set_color(23, red, green, blue);
    rgb_matrix_set_color(28, red, green, blue);
    // Right
    rgb_matrix_set_color(64, red, green, blue);
    rgb_matrix_set_color(59, red, green, blue);
    rgb_matrix_set_color(54, red, green, blue);
    rgb_matrix_set_color(49, red, green, blue);
    rgb_matrix_set_color(44, red, green, blue);
    rgb_matrix_set_color(39, red, green, blue);
  } else if (line == 4) {
    // Left
    /* rgb_matrix_set_color(4, red, green, blue); */
    /* rgb_matrix_set_color(9, red, green, blue); */
    /* rgb_matrix_set_color(14, red, green, blue); */
    /* rgb_matrix_set_color(19, red, green, blue); */
    rgb_matrix_set_color(24, red, green, blue);
    rgb_matrix_set_color(32, red, green, blue);
    rgb_matrix_set_color(33, red, green, blue);
    /* rgb_matrix_set_color(34, red, green, blue); */
    rgb_matrix_set_color(35, red, green, blue);
    // Right
    rgb_matrix_set_color(60, red, green, blue);
    /* rgb_matrix_set_color(55, red, green, blue); */
    /* rgb_matrix_set_color(50, red, green, blue); */
    /* rgb_matrix_set_color(45, red, green, blue); */
    /* rgb_matrix_set_color(40, red, green, blue); */
    rgb_matrix_set_color(68, red, green, blue);
    rgb_matrix_set_color(69, red, green, blue);
    /* rgb_matrix_set_color(70, red, green, blue); */
    rgb_matrix_set_color(71, red, green, blue);
  }
}

// matrix, to set key with. maybe there is a better way but used ascii generator so i can easily set those visually
//    ┌────┬────┬────┬────┬────┬────┬────┐             ┌────┬────┬────┬────┬────┬────┬────┐
//    │ 0  │ 5  │ 10 │ 15 │ 20 │ 25 │ 29 │             │ 65 │ 61 │ 56 │ 51 │ 46 │ 41 │ 36 │
//    ├────┼────┼────┼────┼────┼────┼────┤             ├────┼────┼────┼────┼────┼────┼────┤
//    │ 1  │ 6  │ 11 │ 16 │ 21 │ 26 │ 30 │             │ 66 │ 62 │ 57 │ 52 │ 47 │ 42 │ 37 │
//    ├────┼────┼────┼────┼────┼────┼────┤             ├────┼────┼────┼────┼────┼────┼────┤
//    │ 2  │ 7  │ 12 │ 17 │[22]│ 27 │ 31 │             │ 67 │ 63 │[58]│ 53 │ 48 │ 43 │ 38 │
//    ├────┼────┼────┼────┼────┼────┼────┘             └────┼────┼────┼────┼────┼────┼────┤
//    │ 3  │ 8  │ 13 │ 18 │ 23 │ 28 │                       │ 64 │ 59 │ 54 │ 49 │ 44 │ 39 │
//    ├────┼────┼────┼────┼────┼────┼─────────┐   ┌─────────┼────┼────┼────┼────┼────┼────┤
//    │ 4  │ 9  │ 14 │ 19 │ 24 │    │   35    │   │   71    │    │ 60 │ 55 │ 50 │ 45 │ 40 │
//    └────┴────┴────┴────┴────┼────┼────┬────┤   ├────┬────┼────┼────┴────┴────┴────┴────┘
//                             │ 32 │ 33 │ 34 │   │ 70 │ 69 │ 68 │
//                             └────┴────┴────┘   └────┴────┴────┘
// Runs constantly in the background, in a loop.
bool rgb_matrix_indicators_user(void) {
  if (rgb_matrix_get_flags() & (LED_FLAG_KEYLIGHT | LED_FLAG_MODIFIER)) {
    switch (get_highest_layer(layer_state)) {
    case BEPO:
      rgb_matrix_set_color_all(RGB_OFF);
      // Line 1
      my_set_rgb_matrix_color_line(1,213,144,0);
      // Line 2
      my_set_rgb_matrix_color_line(2,27,213,0);
      my_set_rgb_matrix_color_homerow(206,255,206);
      // Line 3
      my_set_rgb_matrix_color_line(3,0,255,167);
      // Line 4
      my_set_rgb_matrix_color_line(4,0,113,255);
      break;
    case ERGL:
      rgb_matrix_set_color_all(RGB_OFF);
      // ERGOL layer
      rgb_matrix_set_color(34, 0, 113, 255);
      rgb_matrix_set_color(70, 0, 113, 255);
      // Line 1
      my_set_rgb_matrix_color_line(1,213,144,0);
      // Line 2
      my_set_rgb_matrix_color_line(2,27,213,0);
      my_set_rgb_matrix_color_homerow(206,255,206);
      // Line 3
      my_set_rgb_matrix_color_line(3,0,255,167);
      // Line 4
      my_set_rgb_matrix_color_line(4,0,113,255);
      break;
    case QWER:
      rgb_matrix_set_color_all(RGB_OFF);
      // QWERTY layer
      rgb_matrix_set_color(34, 27,213,0);
      rgb_matrix_set_color(70, 27,213,0);
      // Line 1
      my_set_rgb_matrix_color_line(1,213,144,0);
      // Line 2
      my_set_rgb_matrix_color_line(2,27,213,0);
      my_set_rgb_matrix_color_homerow(206,255,206);
      // Line 3
      my_set_rgb_matrix_color_line(3,0,255,167);
      // Line 4
      my_set_rgb_matrix_color_line(4,0,113,255);
      break;
    case NUMB:
      rgb_matrix_set_color_all(RGB_OFF);
      
      rgb_matrix_set_color(0, RGB_RED);

      rgb_matrix_set_color(6, 0, 55, 255); // FX
      rgb_matrix_set_color(11, 0, 55, 255); // FX
      rgb_matrix_set_color(16, 0, 55, 255); // FX
      rgb_matrix_set_color(21, 0, 55, 255); // FX
      rgb_matrix_set_color(26, 0, 55, 255); // FX
      rgb_matrix_set_color(7, 0, 55, 255); // FX
      rgb_matrix_set_color(12, 0, 55, 255); // FX
      rgb_matrix_set_color(17, 0, 55, 255); // FX
      rgb_matrix_set_color(22, 0, 55, 255); // FX
      rgb_matrix_set_color(27, 0, 55, 255); // FX
      rgb_matrix_set_color(8, 0, 55, 255); // FX
      rgb_matrix_set_color(13, 0, 55, 255); // FX

    
      rgb_matrix_set_color(18, 0, 183, 255); // FX
      rgb_matrix_set_color(23, 0, 183, 255); // FX
      rgb_matrix_set_color(28, 0, 183, 255); // FX
      
      rgb_matrix_set_color(60, 255, 255, 255); // 0
      rgb_matrix_set_color(47, 255, 199, 0);     // number
      rgb_matrix_set_color(48, 255, 199, 0);     // number
      rgb_matrix_set_color(49, 255, 199, 0);     // number
      rgb_matrix_set_color(52, 255, 199, 0);     // number
      rgb_matrix_set_color(53, 255, 199, 0);     // number
      rgb_matrix_set_color(54, 255, 199, 0);     // number
      rgb_matrix_set_color(57, 255, 199, 0);     // number
      rgb_matrix_set_color(58, 255, 199, 0);     // number
      rgb_matrix_set_color(59, 255, 199, 0);     // number

      rgb_matrix_set_color(42, 183, 255, 0);     // calc
      rgb_matrix_set_color(43, 183, 255, 0);     // calc
      rgb_matrix_set_color(44, 183, 255, 0);     // calc
      rgb_matrix_set_color(37, 183, 255, 0);     // calc
      rgb_matrix_set_color(38, 183, 255, 0);     // calc
      rgb_matrix_set_color(39, 183, 255, 0);     // calc
      break;
    case SYMB:
      rgb_matrix_set_color_all(RGB_OFF);
      // () {} [] <>
      rgb_matrix_set_color(11, 27, 213, 0);
      rgb_matrix_set_color(16, 27, 213, 0);
      rgb_matrix_set_color(13, 27, 213, 0);
      rgb_matrix_set_color(18, 27, 213, 0);
      rgb_matrix_set_color(57, 27, 213, 0);
      rgb_matrix_set_color(58, 27, 213, 0);
      rgb_matrix_set_color(52, 27, 213, 0);
      rgb_matrix_set_color(53, 27, 213, 0);

      // +-=/*
      rgb_matrix_set_color(23, 29, 204, 67);
      rgb_matrix_set_color(12, 29, 204, 67);
      rgb_matrix_set_color(17, 29, 204, 67);
      rgb_matrix_set_color(22, 29, 204, 67);
      rgb_matrix_set_color(21, 29, 204, 67);
      rgb_matrix_set_color(1, 29, 204, 67); // anti-slash

      // `!~|&%^#
      rgb_matrix_set_color(6, 255, 199, 0);
      rgb_matrix_set_color(7, 255, 199, 0);
      rgb_matrix_set_color(8, 255, 199, 0);
      rgb_matrix_set_color(26, 255, 199, 0);
      rgb_matrix_set_color(27, 255, 199, 0);
      rgb_matrix_set_color(28, 255, 199, 0);
      rgb_matrix_set_color(62, 255, 199, 0);
      rgb_matrix_set_color(42, 255, 199, 0);
      rgb_matrix_set_color(63, 255, 199, 0);
      
      // "'
      rgb_matrix_set_color(43, 204, 202, 29);
      rgb_matrix_set_color(44, 204, 202, 29);

      // :;,.
      rgb_matrix_set_color(59, 29, 120, 204);
      rgb_matrix_set_color(54, 29, 120, 204);
      rgb_matrix_set_color(48, 29, 120, 204);
      rgb_matrix_set_color(49, 29, 120, 204);

      // @ $
      rgb_matrix_set_color(47, 174, 29, 204);
      rgb_matrix_set_color(64, 174, 29, 204);
      
      break;
    case NAVI:
      rgb_matrix_set_color_all(RGB_OFF);

      rgb_matrix_set_color(0, RGB_RED);
      rgb_matrix_set_color(36, RGB_RED);

      // Arrows
      rgb_matrix_set_color(52, 0, 113, 255);
      rgb_matrix_set_color(53, 0, 113, 255);
      rgb_matrix_set_color(58, 0, 113, 255);
      rgb_matrix_set_color(48, 0, 113, 255);
      // Home, End, PgUp, PgDown 
      rgb_matrix_set_color(57, 13, 0, 255);
      rgb_matrix_set_color(47, 13, 0, 255);
      rgb_matrix_set_color(42, 13, 0, 255);
      rgb_matrix_set_color(43, 13, 0, 255);

      // Media
      rgb_matrix_set_color(12, 27, 213, 0);
      rgb_matrix_set_color(17, 27, 213, 0);
      rgb_matrix_set_color(22, 27, 213, 0);

      // Volume
      rgb_matrix_set_color(35, 170, 0, 255);
      rgb_matrix_set_color(71, 170, 0, 255);
      rgb_matrix_set_color(64, 170, 0, 255);

      // PrintScreen
      rgb_matrix_set_color(62, 255, 255, 255);
      break;
    case MOUS:
      rgb_matrix_set_color_all(RGB_OFF);
      // mouse movements
      rgb_matrix_set_color(52, 0, 113, 255);
      rgb_matrix_set_color(53, 0, 113, 255);
      rgb_matrix_set_color(58, 0, 113, 255);
      rgb_matrix_set_color(48, 0, 113, 255);
      // wheel

      rgb_matrix_set_color(7, 13, 0, 255);
      rgb_matrix_set_color(12, 13, 0, 255);
      rgb_matrix_set_color(17, 13, 0, 255);
      rgb_matrix_set_color(22, 13, 0, 255);
      // clicks
      rgb_matrix_set_color(32, 170, 0, 255);
      rgb_matrix_set_color(33, 170, 0, 255);
      rgb_matrix_set_color(68, 170, 0, 255);
      rgb_matrix_set_color(69, 170, 0, 255);
      break;
    default:
      rgb_matrix_set_color_all(RGB_OFF);
      break;
    }
  }
  return false;
}

void caps_word_set_user(bool active) {
  if (active) {
    rgb_matrix_set_color(40, 255, 255, 255);
  } else {
    rgb_matrix_set_color(40, RGB_OFF);
  }
}
