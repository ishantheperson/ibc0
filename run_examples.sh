#!/bin/bash 

C0VM_PATH=$HOME/school/15122/c0vm/c0vm 
IBC0=ibc0 
ERROR=false 

# Check for C0VM and simple 
if [ ! -f $C0VM_PATH ]; then
  echo "Error: Couldn't find C0VM (checked at '$C0VM_PATH')" 
  echo "Try changing C0VM_PATH in this script"

  ERROR=true
fi 

if [ ! -f "./$IBC0" ]; then 
  echo "Error: Couldn't find ./$IBC0"
  echo "Try running make first" 

  ERROR=true 
fi 

if [ "$ERROR" = true ]; then 
  exit 1
fi

for f in examples/*.txt; do 
  echo "Compiling '$f'..."
  ./$IBC0 -b $f 
  echo  
done 

for p in examples/*.bc0; do 
  echo "Running '$p'..."
  time $C0VM_PATH $p 
  echo "C0VM returned $?" 
  echo 
done 
