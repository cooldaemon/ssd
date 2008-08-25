SSD_VSN=1.0 beta 1

EMULATOR=beam
EBIN=$(ROOT)/ebin
INCLUDE=$(ROOT)/include
SCRIPT=$(ROOT)/scripts
CONFIG=$(ROOT)/config

APP_TARGET=$(EBIN)/ssd.app
ELOG_TARGET=$(CONFIG)/elog.config
SH_TARGET=$(SCRIPT)/ssd

ERL_COMPILE_FLAGS += -I$(INCLUDE) -I$(YAWS)/include +warn_unused_vars +nowarn_shadow_vars +warn_unused_import

SOURCES= \
	ssd_ctl.erl \
	ssd_argument.erl \
	ssd_app.erl \
	ssd_sup.erl \
	ssd_store_manager.erl \
	ssd_store.erl \
	ssd_yaws_manager.erl \
	ssd_yaws_mod.erl

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(ELOG_TARGET) $(SH_TARGET)

all: $(OBJECTS)

$(OBJECTS): $(INCLUDE)/ssd.hrl

clean:
	rm -f $(OBJECTS) $(EBIN)/Mnesia.* $(EBIN)/erl_crash.dump

$(EBIN)/%.$(EMULATOR):%.erl
	erlc -pa $(EBIN) -W $(ERL_COMPILE_FLAGS) -I$(INCLUDE) -o$(EBIN) $<

$(APP_TARGET): ssd.app.src Makefile
	sed -e 's;%VSN%;$(SSD_VSN);' $< > $@

$(ELOG_TARGET): elog.config.src Makefile
	sed -e 's;%LOGS_DIR%;$(ROOT)/logs;' $< > $@

$(SH_TARGET): ssd.src
	sed -e 's;%ROOT_DIR%;$(ROOT);' $< > $@
	chmod 755 $@

