.PHONY: build clean test

build:
	dune build @install

test:
	dune runtest

install:
	dune install

uninstall:
	dune uninstall

clean:
	rm -rf _build *.install

# TODO Something like this
# update-version: VERSION=$(shell cat CHANGES.md | grep -E '^[0-9]' | head -n 1 | cut -f1 -d':' )
# update-version:
# 	@echo "Set version to $(VERSION)"
# 	@sed -i 's/^version: .*/version: "$(VERSION)"/' *.opam
# 	@sed -i 's/"\(aws-s3[-a-z]*\)"[ ]*{= .*}/"\1" {= "$(VERSION)" }/' *.opam

# release: VERSION=$(shell cat Changelog | grep -E '^[0-9]' | head -n 1 | cut -f1 -d':')
# release: update-version
# 	opam publish

aws-ec2:
	dune exec aws-gen -- --is-ec2 -i input/ec2/latest/service-2.json -r input/ec2/overrides.json -e input/errors.json -o libraries

# NOTE: This does not include aws-ec2, which is special-cased.
LIBRARIES := \
	aws-autoscaling \
	aws-cloudformation \
	aws-cloudtrail \
	aws-cloudwatch \
	aws-elasticache \
	aws-elasticloadbalancing \
	aws-rds \
	aws-sdb \
	aws-ssm \
	aws-sts \
	aws-s3  \
	aws-ec2 \
	aws-route53 \

.PHONY: $(LIBRARIES)
$(LIBRARIES): aws-%:
	dune exec aws-gen -- -i input/$*/latest/service-2.json -r input/$*/overrides.json -e input/errors.json -o libraries

gen: build aws-ec2 $(LIBRARIES)

# Before this run `dune-release tag <VERSION>`
opam-release:
	dune-release distrib --skip-build --skip-lint --skip-tests
	dune-release publish distrib --verbose
	dune-release opam pkg
	dune-release opam submit
