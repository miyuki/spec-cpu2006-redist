CC = gcc
CFLAGS = -g -Wall -DDEBUG_SCHEDULAR -DTEST_SCHEDULETRAVERSE

SRCS = ScheduleCreater.c ScheduleSorter.c ScheduleTraverse.c ../util/StoreHandledData.c
INCDIRS = ../include

EXE = schedtest

OBJS = $(SRCS:%.c=%.o)

$(EXE): $(OBJS)
	cc -o $@ $(CFLAGS) $(notdir $(OBJS))

%.o : %.c
	cc $(CFLAGS) -c $< $(INCDIRS:%=-I%)

.PHONY: clean

clean:
	rm -f *.o $(EXE)