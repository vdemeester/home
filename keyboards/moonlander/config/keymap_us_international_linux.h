/* US International Linux keycode definitions for bépo AltGr layer
 * These keycodes represent characters accessible via AltGr (Right Alt) on US International layout
 */

#pragma once

#include "quantum.h"

// AltGr keycodes for US International layout on Linux
// Format: RALT(key) or RALT(LSFT(key)) for Shift+AltGr combinations

// Special characters via AltGr
#define US_BRKP  RALT(LSFT(KC_BSLS))  // ¦ broken pipe (Shift+AltGr+\)
#define US_SECT  RALT(KC_S)             // § section sign (AltGr+S)
#define US_OE    RALT(KC_K)             // œ lowercase oe ligature (AltGr+K)
#define US_OE_CAP RALT(LSFT(KC_K))     // Œ uppercase oe ligature (Shift+AltGr+K)
#define US_PND   RALT(LSFT(KC_4))      // £ pound sterling (Shift+AltGr+4)
#define US_RSQT  RALT(KC_RBRC)         // ' right single quote (AltGr+])
#define US_LSQT  RALT(KC_LBRC)         // ' left single quote (AltGr+[)
#define US_DEG   RALT(LSFT(KC_8))      // ° degree (Shift+AltGr+8)
#define US_MICR  RALT(KC_M)             // µ micro (AltGr+M)
#define US_DAGG  RALT(KC_T)             // † dagger (AltGr+T)
#define US_DDAG  RALT(LSFT(KC_7))      // ‡ double dagger (Shift+AltGr+7)
#define US_AE_CAP RALT(LSFT(KC_Z))     // Æ uppercase ae ligature (Shift+AltGr+Z)
#define US_COPY  RALT(KC_C)             // © copyright (AltGr+C)
#define US_TM    RALT(LSFT(KC_2))      // ™ trademark (Shift+AltGr+2)
#define US_SS    RALT(KC_S)             // ß eszett/sharp s (AltGr+S)
#define US_REGD  RALT(KC_R)             // ® registered (AltGr+R)
