OCSIGENSERVER     := ocsigenserver
OCSIGENSERVER.OPT := ocsigenserver.opt

ifneq ($(DEBUG),yes)
  DUNE_OPTIONS = --profile release
endif

.PHONY: all byte opt

DIST_DIRS          := $(ETCDIR) $(DATADIR) $(LIBDIR) $(LOGDIR) \
					  $(FILESDIR)/avatars/tmp $(ELIOMSTATICDIR) \
					  $(shell dirname $(CMDPIPE))
JS_PREFIX          := $(TEST_PREFIX)$(ELIOMSTATICDIR)/$(PROJECT_NAME)

all:: byte opt

DIST_FILES = $(ELIOMSTATICDIR)/$(PROJECT_NAME).js $(LIBDIR)/$(PROJECT_NAME).cma

.PHONY: test.byte test.opt 

test.byte:: byte | $(addprefix $(TEST_PREFIX),$(DIST_DIRS))
	dune exec ./h42n42.exe

test.opt:: opt | $(addprefix $(TEST_PREFIX),$(DIST_DIRS))
	dune exec ./h42n42.exe

test.static.byte: test.byte

test.static.opt: test.opt

$(addprefix $(TEST_PREFIX), $(DIST_DIRS)):
	mkdir -p $@

css:
	cp -rf $(LOCAL_STATIC)/css $(TEST_PREFIX)$(ELIOMSTATICDIR)

.PHONY: install install.static install.lib run

install: all install.static | $(addprefix $(PREFIX),$(DATADIR) $(LOGDIR) $(shell dirname $(CMDPIPE)))
	dune install

install.static: $(TEST_PREFIX)$(ELIOMSTATICDIR)/$(PROJECT_NAME).js | $(PREFIX)$(STATICDIR) $(PREFIX)$(ELIOMSTATICDIR)
	cp -r $(LOCAL_STATIC)/css $(PREFIX)$(FILESDIR)
	HASH=`md5sum _build/default/client/$(PROJECT_NAME).bc.js | cut -d ' ' -f 1` && \
	install $(addprefix -o ,$(WWWUSER)) $(JS_PREFIX)_$$HASH.js $(PREFIX)$(ELIOMSTATICDIR) && \
	ln -sf $(PROJECT_NAME)_$$HASH.js $(PREFIX)$(ELIOMSTATICDIR)/$(PROJECT_NAME).js
	[ -z $(WWWUSER) ] || chown -R $(WWWUSER) $(PREFIX)$(FILESDIR)

.PHONY:
print-install-files:
	@echo $(PREFIX)$(LIBDIR)
	@echo $(PREFIX)$(ELIOMSTATICDIR)
	@echo $(PREFIX)$(ETCDIR)

$(addprefix $(PREFIX),$(ETCDIR) $(LIBDIR)):
	install -d $@
$(addprefix $(PREFIX),$(DATADIR) $(LOGDIR) $(ELIOMSTATICDIR) $(shell dirname $(CMDPIPE))):
	install $(addprefix -o ,$(WWWUSER)) -d $@

.PHONY: gen-dune config-files

config-files: | $(TEST_PREFIX)$(ELIOMSTATICDIR) $(TEST_PREFIX)$(LIBDIR)
	HASH=`md5sum _build/default/client/$(PROJECT_NAME).bc.js | cut -d ' ' -f 1` && \
	cp -f _build/default/client/$(PROJECT_NAME).bc.js $(JS_PREFIX)_$$HASH.js && \
	ln -sf $(PROJECT_NAME)_$$HASH.js $(JS_PREFIX).js
	cp -f _build/default/$(PROJECT_NAME).cm* $(TEST_PREFIX)$(LIBDIR)/

all::
	$(ENV_PSQL) dune build $(DUNE_OPTIONS) @install @$(PROJECT_NAME)

js::
	$(ENV_PSQL) dune build $(DUNE_OPTIONS) client/$(PROJECT_NAME).bc.js

byte:: js
	$(ENV_PSQL) dune build $(DUNE_OPTIONS) $(PROJECT_NAME).bc
	make config-files PROJECT_NAME=$(PROJECT_NAME)
	make css

opt:: js
	$(ENV_PSQL) dune build $(DUNE_OPTIONS) $(PROJECT_NAME).exe
	make config-files PROJECT_NAME=$(PROJECT_NAME)
	make css

run:
	$(PREFIX)bin/$(PROJECT_NAME)

clean::
	dune clean
	rm -rf $(TEST_PREFIX)
