# OASIS_START
# DO NOT EDIT (digest: a3c674b4239234cbbe53afe090018954)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

.PHONY: aws_ec2
aws_ec2:
	./aws_gen.native --is-ec2 -i input/ec2/latest/service-2.json -r input/ec2/overrides.json -e input/errors.json -o libraries
	cd libraries/ec2 && oasis setup

# NOTE: This does not include aws_ec2, which is special-cased.
LIBRARIES := \
	aws_autoscaling \
	aws_cloudformation \
	aws_cloudtrail \
	aws_elasticache \
	aws_elasticloadbalancing \
	aws_rds \
	aws_sdb \
	aws_ssm \
	aws_sts \

.PHONY: $(LIBRARIES)
$(LIBRARIES): aws_%:
	./aws_gen.native -i input/$*/latest/service-2.json -r input/$*/overrides.json -e input/errors.json -o libraries
	cd libraries/$* && oasis setup

gen: all aws_ec2 $(LIBRARIES)

test-libraries: gen reinstall
	$(MAKE) -C libraries configure CONFIGUREFLAGS=--enable-tests test

install-libraries: gen reinstall
	$(MAKE) -C libraries build reinstall

clean-libraries:
	$(MAKE) -C libraries clean

enable-async:
	./configure --enable-async
	$(MAKE) -C libraries configure CONFIGUREFLAGS=--enable-async

enable-lwt:
	./configure --enable-lwt
	$(MAKE) -C libraries configure CONFIGUREFLAGS=--enable-lwt

disable-async:
	./configure --disable-async
	$(MAKE) -C libraries configure CONFIGUREFLAGS=--disable-async

disable-lwt:
	./configure --disable-lwt
	$(MAKE) -C libraries configure CONFIGUREFLAGS=--disable-lwt
