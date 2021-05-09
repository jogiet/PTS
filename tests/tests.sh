#!/bin/bash

pts=./pts.exe
max_time=5s

solved=0
failed=0
timeout=0

RED="\e[0;91m"
GREEN="\e[0;92m"
BOLD="\e[1m"
RESET="\e[0m"
BLUE="\e[0;94m"
fill="                                                     "

print_result () {
	if [[ $total == 0 ]]
	then
		echo -e "bench: $type ${RED}${BOLD}empty${RESET}"
		return
	fi
	if [[ $failed == 0 ]] && [[ $timeout  == 0 ]]
	then
		echo -e "bench: $type ${GREEN}${BOLD}ALL tests pass (${total})${RESET}"
	else
		prct_fail=`expr 100 \* $failed / $total`;
		prct_to=`expr 100 \* $timeout / $total`;
		echo -e "bench: $bench${RED}${BOLD} ERROR${RESET}"
		echo -e "${RED}${failed}/${total} \ttest fail   : ${prct_fail}%${RESET}"
		echo -e "${RED}${timeout}/${total}\ttest timeout: ${prct_to}%${RESET}"
	fi
	solved=0
	failed=0
	total=0
	timeout=0
}

treat_bench () {
	bench=tests
	total=$(find $bench -name "*.f" -o -name "*.fw" -o -name "*.cc" | wc -l)
	for file in $(find $bench -name "*.f" -o -name "*.fw" -o -name "*.cc")
	do
		pct=`expr 100 \* $solved / $total`;
		echo -ne "\r$pct%      $file$fill"
		timeout $max_time $pts $file > tmp
		res=$?
		if [[ $res == 124 ]]
		then
			echo -e "\r    ${RED}TO${RESET}"
			timeout=$((timeout+1))
			continue
		elif [[ $res != 0 ]]
		then
			echo -e "\r    ${RED}KO($res)${RESET}"
			failed=$((failed+1))
			cat tmp
			echo -e "\n------------------------------------------------------------"
		fi
		solved=$(($solved+1))
	done
	echo -ne "\r$fill\r"
	rm -f tmp
	print_result
}

treat_bench

