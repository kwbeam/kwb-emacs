#!/bin/bash

rm -f init.el
rm -f elpa

ln -s init.$1.el init.el
ln -s elpa.$1 elpa
