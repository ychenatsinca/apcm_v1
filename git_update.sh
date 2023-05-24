#!/bin/sh

#init git
#git init

#add files
git add  *.R *

#commit files
#git commit -m "The first commit of Automated Phenology Classification Model (APCM) version 1.0 "
git commit -m "add resource files "



#set the origin, only for the first time
#git remote add origin git@github.com:ychenatsinca/apcm_v1.git
#git remote add origin git@github.com:ychenatsinca/apcm_v1.git

#add branch name, here is main
#git branch -M main

#push commit files to the server/origin as master or branch
git push -u origin main

