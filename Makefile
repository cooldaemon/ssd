APPNAME=ssd
SUB_DIRECTORIES=src
YAWS=/usr/local/lib/yaws

ifndef ROOT
  ROOT=$(shell pwd)
endif

all: subdirs

subdirs:
	@for d in $(SUB_DIRECTORIES); do \
		(cd $$d; ROOT=$(ROOT) YAWS=$(YAWS) $(MAKE)); \
	done

clean:
	@for d in $(SUB_DIRECTORIES); do \
		(cd $$d; ROOT=$(ROOT) $(MAKE) clean); \
	done
	rm -rf Mnesia.* erl_crash.dump logs/* test/*.beam .yaws

run_test:
	@for d in $(SUB_DIRECTORIES); do \
		(cd $$d; ROOT=$(ROOT) YAWS=$(YAWS) ERL_COMPILE_FLAGS=+debug_info \
			$(MAKE)); \
	done
	run_test \
		-dir $(ROOT) -logdir $(ROOT)/logs \
		-cover $(ROOT)/config/ssd.coverspec \
		-pa $(ROOT)/ebin \
		-pa $(YAES)/ebin

