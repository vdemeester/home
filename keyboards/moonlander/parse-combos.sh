#!/usr/bin/env bash
# Parse combo definitions from keymap.c and generate YAML format for keymap-drawer
#
# This script maps QMK keycodes to their visual positions in the keymap-drawer output.
# The positions are derived from the QWERTY layer layout in keymap.c.
#
# HOW POSITIONS WERE DETERMINED:
# 1. Convert QMK keymap to JSON (from QMK firmware directory):
#    $ qmk c2json --no-cpp -kb zsa/moonlander -km vincent > moonlander.json
#
# 2. Parse JSON to YAML with keymap-drawer (14 columns for Moonlander):
#    $ keymap parse -c 14 -q moonlander.json > moonlander.yaml
#
# 3. The resulting YAML contains the layout array where each key has a sequential
#    index (0-71 in row-major order). Match keycodes to their positions by looking
#    at the QWERTY layer (L2) array in the JSON output.
#
# 4. For combo definitions in keymap.c, the key positions in combo arrays correspond
#    to these visual indices. Example from keymap.c:
#      const uint16_t qwer_combo[] = {KC_Q, KC_W, COMBO_END};
#    Maps to positions [15, 16] where KC_Q=position 15, KC_W=position 16
#
# NOTE: Home row mod keys (HM_*) need special mapping since they differ between
# layers (e.g., HM_GUI_A on QWERTY vs Bépo), but physical position stays the same.

set -euo pipefail

# Map of QMK keycodes to visual positions (based on QWERTY layer)
# The Moonlander has 72 keys, indexed 0-71 in row-major order
declare -A POS

# Row 0 (top row) - all XXXXXXX
for i in {0..13}; do POS[XXXXXXX_$i]=$i; done

# Row 1
POS[KC_TAB]=14
POS[KC_Q]=15; POS[KC_W]=16; POS[KC_E]=17; POS[KC_R]=18; POS[KC_T]=19
POS[KC_Y]=22; POS[KC_U]=23; POS[KC_I]=24; POS[KC_O]=25; POS[KC_P]=26; POS[KC_LBRC]=27

# Row 2
POS[NUMWORD]=28
POS[HM_GUI_A]=29; POS[HM_ALT_S]=30; POS[HM_SFT_D]=31; POS[HM_CTL_F]=32; POS[HM_HYP_G]=33
POS[HM_HYP_H]=36; POS[HM_CTL_J]=37; POS[HM_SFT_K]=38; POS[HM_ALT_L]=39; POS[HM_GUI_SCLN]=40; POS[KC_QUOT]=41

# Row 3
POS[KC_GRV]=42
POS[KC_Z]=43; POS[KC_X]=44; POS[KC_C]=45; POS[KC_V]=46; POS[KC_B]=47
POS[KC_N]=48; POS[KC_M]=49; POS[KC_COMM]=50; POS[KC_DOT]=51; POS[KC_SLSH]=52; POS[KC_RBRC]=53

# Row 4 (bottom row before thumb cluster)
for i in {54..57}; do POS[XXXXXXX_row4_$i]=$i; done
POS[KC_DEL]=58
POS[QK_REP]=59
POS[QK_AREP]=60
POS[KC_RALT]=61
for i in {62..65}; do POS[XXXXXXX_row4_$i]=$i; done

# Row 5 (thumb cluster)
POS["LT(NUMB,KC_SPC)"]=66
POS["LT(NAVI,KC_BSPC)"]=67
POS["OS_LSFT"]=70
POS["LT(SYMB,KC_ENT)"]=71

# Also map for Bépo layer equivalents
POS[HM_ALT_R]=39  # Bépo R is same position as QWERTY L
POS[HM_GUI_N]=40  # Bépo N is same position as QWERTY SCLN

get_pos() {
    local key=$1
    echo "${POS[$key]:-}"
}

echo "combos:"
echo "  # Layer switching combos"
echo "  - { p: [$(get_pos 'LT(NAVI,KC_BSPC)'), $(get_pos 'OS_LSFT')], k: \"→ Bépo\", l: [L1, L2], draw_separate: true }"
echo "  - { p: [$(get_pos 'LT(NUMB,KC_SPC)'), $(get_pos 'LT(SYMB,KC_ENT)')], k: \"→ ErgoL\", l: [L0, L2], draw_separate: true }"
echo "  - { p: [$(get_pos 'KC_DEL'), $(get_pos 'KC_RALT')], k: \"→ QWERTY\", l: [L0, L1], draw_separate: true }"
echo "  - { p: [$(get_pos 'KC_Q'), $(get_pos 'KC_R')], k: \"⇄ Mouse\", draw_separate: true }"
echo ""

echo "  # Escape combos (layer-specific)"
echo "  - { p: [$(get_pos 'HM_ALT_R'), $(get_pos 'HM_GUI_N')], k: ESC, l: [L0] }"
echo "  - { p: [$(get_pos 'HM_ALT_L'), $(get_pos 'HM_GUI_SCLN')], k: ESC, l: [L2] }"
echo ""

echo "  # Special character combos (available on all layers)"
echo "  - { p: [$(get_pos 'KC_Q'), $(get_pos 'KC_W')], k: \"|\" }"
echo "  - { p: [$(get_pos 'KC_W'), $(get_pos 'KC_E')], k: \"@\" }"
echo "  - { p: [$(get_pos 'KC_E'), $(get_pos 'KC_R')], k: \"#\" }"
echo "  - { p: [$(get_pos 'KC_R'), $(get_pos 'KC_T')], k: \"&\" }"
echo "  - { p: [$(get_pos 'KC_R'), $(get_pos 'HM_CTL_F')], k: \"\$\" }"
echo "  - { p: [$(get_pos 'KC_E'), $(get_pos 'HM_SFT_D')], k: \"/\" }"
printf '  - { p: [%s, %s], k: "\\\\" }\n' "$(get_pos 'HM_SFT_D')" "$(get_pos 'KC_C')"
echo "  - { p: [$(get_pos 'KC_W'), $(get_pos 'HM_ALT_S')], k: \"-\" }"
echo "  - { p: [$(get_pos 'HM_CTL_F'), $(get_pos 'KC_V')], k: \"_\" }"
echo "  - { p: [$(get_pos 'HM_ALT_S'), $(get_pos 'KC_X')], k: \"=\" }"
echo ""

echo "  # Bracket combos (available on all layers)"
echo "  - { p: [$(get_pos 'KC_I'), $(get_pos 'HM_SFT_K')], k: \"(\" }"
echo "  - { p: [$(get_pos 'HM_SFT_K'), $(get_pos 'KC_COMM')], k: \")\" }"
echo "  - { p: [$(get_pos 'KC_U'), $(get_pos 'HM_CTL_J')], k: \"{\" }"
echo "  - { p: [$(get_pos 'HM_CTL_J'), $(get_pos 'KC_M')], k: \"}\" }"
echo "  - { p: [$(get_pos 'KC_O'), $(get_pos 'HM_ALT_L')], k: \"[\" }"
echo "  - { p: [$(get_pos 'HM_ALT_L'), $(get_pos 'KC_DOT')], k: \"]\" }"
echo "  - { p: [$(get_pos 'KC_U'), $(get_pos 'KC_I')], k: \"<\" }"
echo "  - { p: [$(get_pos 'KC_I'), $(get_pos 'KC_O')], k: \">\" }"
echo ""

echo "  # Leader key combo (available on all layers)"
echo "  - { p: [$(get_pos 'HM_SFT_D'), $(get_pos 'HM_CTL_F')], k: LEADER }"
