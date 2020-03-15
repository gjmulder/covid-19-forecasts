#!/bin/sh


FRAMERATE=0.5
STATIC_DUR=5
FADE_DUR=10

R BATCH --vanilla -f plots-covid.R

NUM_IMGS=`ls *_deaths_norm_aged.png | wc -l`
NUM_SECS=`echo "$NUM_IMGS / $FRAMERATE" | bc`
PLAY_DUR=`echo "$NUM_SECS + $STATIC_DUR" | bc`
TOT_TIME=`echo "$PLAY_DUR + $FADE_DUR" | bc`

ffmpeg -y -i Lacrimosa.mp3 -af "afade=t=out:st=$PLAY_DUR:d=$FADE_DUR" audio1.mp3
ffmpeg -y -ss 0 -i audio1.mp3 -to $TOT_TIME -acodec copy audio2.mp3
ffmpeg -y -framerate $FRAMERATE -i %d_deaths_norm_aged.png video.avi
ffmpeg -y -i video.avi -i audio2.mp3 -c:v copy -c:a aac -strict experimental deaths_norm_aged.mp4
rm *_deaths_norm_aged.png audio1.mp3 audio2.mp3 video.avi
