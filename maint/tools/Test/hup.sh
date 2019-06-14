#!/usr/bin/sh

myVar="a"

watdoetie () {
   myVarMain="${myVar}"
   local myVar="${myVar}"
   echo "${myVarMain}"
   echo "${myVar}"
}

watdoetie
