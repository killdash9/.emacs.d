#!/bin/bash

#This is used to tame helm-ag results from minimized javascript files
ag "$@" | cut -b1-400