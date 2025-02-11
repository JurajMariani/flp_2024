all:
	swipl -q -o flp23-log -g main -c input2.pl proj2.pl

big_stack:
	swipl -q -G16g -o flp23-log -g main -c input2.pl proj2.pl

ver8random:
	swipl -o flp23-log -g main -c input2.pl proj2random.pl

ver8random_big_stack:
	swipl --stack_limit=16g -o flp23-log -g main -c input2.pl proj2random.pl

