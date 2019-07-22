#!/bin/bash 

# Path to C0VM executable 
C0VM_PATH=$HOME/school/15122/c0vm/c0vm 

# Path to ibc0 
IBC0=ibc0 

# Check for C0VM and simple 
if [ ! -f $C0VM_PATH ]; then
  echo "Error: Couldn't find C0VM (checked at '$C0VM_PATH')" 
  echo "Try changing C0VM_PATH in this script"

  exit 1
fi 

if [ ! -f "./$IBC0" ]; then 
  echo "Error: Couldn't find ./$IBC0"
  read -p "Run make now? [y|n] " choice 
  case "$choice" in 
    y|y ) make 
          if [ ! -f "./$IBC0" ]; then 
            echo "Error: compilation appears to have failed"
            echo "Try verifying that IBC0 is correct in this script"

            exit 1
          fi 
          ;; 

    * ) exit 1 ;;
  esac 
fi 


for f in examples/*.txt; do 
  echo "Compiling '$f'..."
  ./$IBC0 -b $f 
  echo  
done 

for p in examples/*.bc0; do 
  echo "Running '$p'..."
  $C0VM_PATH $p 
  echo "C0VM returned $?" 
  echo 
done 
