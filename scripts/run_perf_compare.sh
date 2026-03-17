#!/usr/bin/env bash
set -euo pipefail

TIMEFORMAT='%R'

echo ""
echo "=== Running POS-IL-det ==="
{ pos_il_det_time=$( { time ./p4perf run -s spec-concrete -i p4c/p4include -e excludes \
    -p4-dir p4c/testdata/p4_16_samples -rel Program_inst -il -det \
    > output_pos_il_det.log; } 2>&1 ); } 2>/dev/null
echo "POS-IL-det runtime: ${pos_il_det_time}s"
echo "POS-IL-det runtime: ${pos_il_det_time}s" >> time.log

echo ""
echo "=== Running POS-IL ==="
{ pos_il_time=$( { time ./p4perf run -s spec-concrete -i p4c/p4include -e excludes \
    -p4-dir p4c/testdata/p4_16_samples -rel Program_inst -il \
    > output_pos_il.log; } 2>&1 ); } 2>/dev/null
echo "POS-IL runtime: ${pos_il_time}s"
echo "POS-IL runtime: ${pos_il_time}s" >> time.log

echo "=== Running POS-SL-det ==="
{ pos_sl_det_time=$( { time ./p4perf run -s spec-concrete -i p4c/p4include -e excludes \
    -p4-dir p4c/testdata/p4_16_samples -rel Program_inst -sl -det \
    > output_pos_sl_det.log; } 2>&1 ); } 2>/dev/null
echo "POS-SL-det runtime: ${pos_sl_det_time}s"
echo "POS-SL-det runtime: ${pos_sl_det_time}s" >> time.log

echo "=== Running POS-SL ==="
{ pos_sl_time=$( { time ./p4perf run -s spec-concrete -i p4c/p4include -e excludes \
    -p4-dir p4c/testdata/p4_16_samples -rel Program_inst -sl \
    > output_pos_sl.log; } 2>&1 ); } 2>/dev/null
echo "POS-SL runtime: ${pos_sl_time}s" >> time.log

echo ""
echo "=== Running V1MODEL+-IL-det ==="
{ v1model_il_det_time=$( { time ./p4perf sim -s spec-concrete -i p4c/p4include -e excludes \
    -p4-dir p4c/testdata/p4_16_samples \
    -stf-dir testdata/p4testgen \
    -p patches/v1model \
    -arch v1model -il -det \
    > output_v1model_il_det.log; } 2>&1 ); } 2>/dev/null
echo "V1MODEL+-IL-det runtime: ${v1model_il_det_time}s"
echo "V1MODEL+-IL-det runtime: ${v1model_il_det_time}s" >> time.log

echo ""
echo "=== Running V1MODEL+-IL ==="
{ v1model_il_time=$( { time ./p4perf sim -s spec-concrete -i p4c/p4include -e excludes \
    -p4-dir p4c/testdata/p4_16_samples \
    -stf-dir testdata/p4testgen \
    -p patches/v1model \
    -arch v1model -il \
    > output_v1model_il.log; } 2>&1 ); } 2>/dev/null
echo "V1MODEL+-IL runtime: ${v1model_il_time}s"
echo "V1MODEL+-IL runtime: ${v1model_il_time}s" >> time.log

echo ""
echo "=== Running V1MODEL+-SL-det ==="
{ v1model_sl_det_time=$( { time ./p4perf sim -s spec-concrete -i p4c/p4include -e excludes \
    -p4-dir p4c/testdata/p4_16_samples \
    -stf-dir testdata/p4testgen \
    -p patches/v1model \
    -arch v1model -sl -det \
    > output_v1model_sl_det.log; } 2>&1 ); } 2>/dev/null
echo "V1MODEL+-SL-det runtime: ${v1model_sl_det_time}s"
echo "V1MODEL+-SL-det runtime: ${v1model_sl_det_time}s" >> time.log

echo ""
echo "=== Running V1MODEL+-SL ==="
{ v1model_sl_time=$( { time ./p4perf sim -s spec-concrete -i p4c/p4include -e excludes \
    -p4-dir p4c/testdata/p4_16_samples \
    -stf-dir testdata/p4testgen \
    -p patches/v1model \
    -arch v1model -sl \
    > output_v1model_sl.log; } 2>&1 ); } 2>/dev/null
echo "V1MODEL+-SL runtime: ${v1model_sl_time}s"
echo "V1MODEL+-SL runtime: ${v1model_sl_time}s" >> time.log
