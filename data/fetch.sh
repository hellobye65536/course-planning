#!/bin/bash
cd "$(dirname "$0")"
cat subjects.txt | while read subj; do
	wget -O "pages/$subj.html" "https://ucalendar.uwaterloo.ca/2324/COURSE/course-$subj.html" &
done
