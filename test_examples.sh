#!/bin/bash

rouge='\e[0;31m'
vert='\e[1;32m'
bleu='\e[1;34m'
cyan='\e[1;36m'
orange='\e[0;33m'
neutre='\e[0;m'

exit_code=0

make re
cd vm
make re
cd ..

echo
echo
echo -e "${bleu}./glados examples/add1.hyd"
./glados examples/add1.hyd
EXPECTED_OUTPUT="7"
ACTUAL_OUTPUT=$(./vm/hydreigon output.hyd)
if [[ "$ACTUAL_OUTPUT" == "$EXPECTED_OUTPUT" ]]; then
  echo -e "${vert}Test passed: the output is correct."
else
  echo -e "${rouge}Test failed: the output is incorrect."
  echo -e "${rouge}Expected output: $EXPECTED_OUTPUT"
  echo -e "${rouge}Actual output: $ACTUAL_OUTPUT"
  exit_code=1
fi

echo
echo
echo -e "${bleu}./glados examples/sub1.hyd"
./glados examples/sub1.hyd
EXPECTED_OUTPUT="8"
ACTUAL_OUTPUT=$(./vm/hydreigon output.hyd)
if [[ "$ACTUAL_OUTPUT" == "$EXPECTED_OUTPUT" ]]; then
  echo -e "${vert}Test passed: the output is correct."
else
  echo -e "${rouge}Test failed: the output is incorrect."
  echo -e "${rouge}Expected output: $EXPECTED_OUTPUT"
  echo -e "${rouge}Actual output: $ACTUAL_OUTPUT"
  exit_code=1
fi

echo
echo
echo -e "${bleu}./glados examples/div1.hyd"
./glados examples/div1.hyd
EXPECTED_OUTPUT="5"
ACTUAL_OUTPUT=$(./vm/hydreigon output.hyd)
if [[ "$ACTUAL_OUTPUT" == "$EXPECTED_OUTPUT" ]]; then
  echo -e "${vert}Test passed: the output is correct."
else
  echo -e "${rouge}Test failed: the output is incorrect."
  echo -e "${rouge}Expected output: $EXPECTED_OUTPUT"
  echo -e "${rouge}Actual output: $ACTUAL_OUTPUT"
  exit_code=1
fi

echo
echo
echo -e "${bleu}./glados examples/mod1.hyd"
./glados examples/mod1.hyd
EXPECTED_OUTPUT="1"
ACTUAL_OUTPUT=$(./vm/hydreigon output.hyd)
if [[ "$ACTUAL_OUTPUT" == "$EXPECTED_OUTPUT" ]]; then
  echo -e "${vert}Test passed: the output is correct."
else
  echo -e "${rouge}Test failed: the output is incorrect."
  echo -e "${rouge}Expected output: $EXPECTED_OUTPUT"
  echo -e "${rouge}Actual output: $ACTUAL_OUTPUT"
  exit_code=1
fi

echo
echo
echo -e "${bleu}./glados examples/mul1.hyd"
./glados examples/mul1.hyd
EXPECTED_OUTPUT="16"
ACTUAL_OUTPUT=$(./vm/hydreigon output.hyd)
if [[ "$ACTUAL_OUTPUT" == "$EXPECTED_OUTPUT" ]]; then
  echo -e "${vert}Test passed: the output is correct."
else
  echo -e "${rouge}Test failed: the output is incorrect."
  echo -e "${rouge}Expected output: $EXPECTED_OUTPUT"
  echo -e "${rouge}Actual output: $ACTUAL_OUTPUT"
  exit_code=1
fi

echo
echo
echo -e "${bleu}./glados examples/string1.hyd"
./glados examples/string1.hyd
EXPECTED_OUTPUT="Hello World !"
ACTUAL_OUTPUT=$(./vm/hydreigon output.hyd)
if [[ "$ACTUAL_OUTPUT" == "$EXPECTED_OUTPUT" ]]; then
  echo -e "${vert}Test passed: the output is correct."
else
  echo -e "${rouge}Test failed: the output is incorrect."
  echo -e "${rouge}Expected output: $EXPECTED_OUTPUT"
  echo -e "${rouge}Actual output: $ACTUAL_OUTPUT"
  exit_code=1
fi

echo
echo
echo -e "${bleu}./glados examples/add2.hyd"
./glados examples/add2.hyd
EXPECTED_OUTPUT="45"
ACTUAL_OUTPUT=$(./vm/hydreigon output.hyd)
if [[ "$ACTUAL_OUTPUT" == "$EXPECTED_OUTPUT" ]]; then
  echo -e "${vert}Test passed: the output is correct."
else
  echo -e "${rouge}Test failed: the output is incorrect."
  echo -e "${rouge}Expected output: $EXPECTED_OUTPUT"
  echo -e "${rouge}Actual output: $ACTUAL_OUTPUT"
  exit_code=1
fi

echo
echo
echo -e "${bleu}./glados examples/sub2.hyd"
./glados examples/sub2.hyd
EXPECTED_OUTPUT="26"
ACTUAL_OUTPUT=$(./vm/hydreigon output.hyd)
if [[ "$ACTUAL_OUTPUT" == "$EXPECTED_OUTPUT" ]]; then
  echo -e "${vert}Test passed: the output is correct."
else
  echo -e "${rouge}Test failed: the output is incorrect."
  echo -e "${rouge}Expected output: $EXPECTED_OUTPUT"
  echo -e "${rouge}Actual output: $ACTUAL_OUTPUT"
  exit_code=1
fi

echo
echo
echo -e "${bleu}./glados examples/div2.hyd"
./glados examples/div2.hyd
EXPECTED_OUTPUT="5"
ACTUAL_OUTPUT=$(./vm/hydreigon output.hyd)
if [[ "$ACTUAL_OUTPUT" == "$EXPECTED_OUTPUT" ]]; then
  echo -e "${vert}Test passed: the output is correct."
else
  echo -e "${rouge}Test failed: the output is incorrect."
  echo -e "${rouge}Expected output: $EXPECTED_OUTPUT"
  echo -e "${rouge}Actual output: $ACTUAL_OUTPUT"
  exit_code=1
fi

echo
echo
echo -e "${bleu}./glados examples/mod2.hyd"
./glados examples/mod2.hyd
EXPECTED_OUTPUT="0"
ACTUAL_OUTPUT=$(./vm/hydreigon output.hyd)
if [[ "$ACTUAL_OUTPUT" == "$EXPECTED_OUTPUT" ]]; then
  echo -e "${vert}Test passed: the output is correct."
else
  echo -e "${rouge}Test failed: the output is incorrect."
  echo -e "${rouge}Expected output: $EXPECTED_OUTPUT"
  echo -e "${rouge}Actual output: $ACTUAL_OUTPUT"
  exit_code=1
fi

echo
echo
echo -e "${bleu}./glados examples/mul2.hyd"
./glados examples/mul2.hyd
EXPECTED_OUTPUT="25"
ACTUAL_OUTPUT=$(./vm/hydreigon output.hyd)
if [[ "$ACTUAL_OUTPUT" == "$EXPECTED_OUTPUT" ]]; then
  echo -e "${vert}Test passed: the output is correct."
else
  echo -e "${rouge}Test failed: the output is incorrect."
  echo -e "${rouge}Expected output: $EXPECTED_OUTPUT"
  echo -e "${rouge}Actual output: $ACTUAL_OUTPUT"
  exit_code=1
fi

echo
echo
echo -e "${bleu}./glados examples/string2.hyd"
./glados examples/string2.hyd
EXPECTED_OUTPUT=" Goodbye World !"
ACTUAL_OUTPUT=$(./vm/hydreigon output.hyd)
if [[ "$ACTUAL_OUTPUT" == "$EXPECTED_OUTPUT" ]]; then
  echo -e "${vert}Test passed: the output is correct."
else
  echo -e "${rouge}Test failed: the output is incorrect."
  echo -e "${rouge}Expected output: $EXPECTED_OUTPUT"
  echo -e "${rouge}Actual output: $ACTUAL_OUTPUT"
  exit_code=1
fi

echo
echo
echo -e "${bleu}./glados examples/commentary.hyd"
./glados examples/commentary.hyd
EXPECTED_OUTPUT="22"
ACTUAL_OUTPUT=$(./vm/hydreigon output.hyd)
if [[ "$ACTUAL_OUTPUT" == "$EXPECTED_OUTPUT" ]]; then
  echo -e "${vert}Test passed: the output is correct."
else
  echo -e "${rouge}Test failed: the output is incorrect."
  echo -e "${rouge}Expected output: $EXPECTED_OUTPUT"
  echo -e "${rouge}Actual output: $ACTUAL_OUTPUT"
  exit_code=1
fi

echo -e "${neutre}"

exit $exit_code
