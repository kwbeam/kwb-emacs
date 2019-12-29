#!/bin/bash

rm init.el
rm elpa

ln -s init.$1.el init.el
ln -s elpa.$1 elpa
