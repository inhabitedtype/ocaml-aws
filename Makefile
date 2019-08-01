.PHONY: build clean test update-version

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

update-version:
	scripts/update-version

.PHONY: endpoints

endpoints:
	dune exec endpoint-gen -- -i input/endpoints.json -o lib

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
	aws-route53 \
	aws-sqs \

.PHONY: $(LIBRARIES)
$(LIBRARIES): aws-%:
	dune exec aws-gen -- -i input/$*/latest/service-2.json -r input/$*/overrides.json -e input/errors.json -o libraries

gen: build aws-ec2 $(LIBRARIES)

# mv libraries/ssm/aws_ssm.opam .
# mv libraries/cloudwatch/aws_cloudwatch.opam .
# mv libraries/s3/aws_s3.opam .
# mv libraries/elasticloadbalancing/aws_elasticloadbalancing.opam .
# mv libraries/cloudtrail/aws_cloudtrail.opam .
# mv libraries/sdb/aws_sdb.opam .
# mv libraries/autoscaling/aws_autoscaling.opam .
# mv libraries/cloudformation/aws_cloudformation.opam .
# mv libraries/elasticache/aws_elasticache.opam .
# mv libraries/rds/aws_rds.opam .
# mv libraries/sts/aws_sts.opam .
# mv libraries/route53/aws_route53.opam .
# mv libraries/ec2/aws_ec2.opam .

# TODO Something like this
# update-version: VERSION=$(shell cat CHANGES.md | grep -E '^[0-9]' | head -n 1 | cut -f1 -d':' )
# update-version:
# 	@echo "Set version to $(VERSION)"
# 	@sed -i 's/^version: .*/version: "$(VERSION)"/' *.opam
# 	@sed -i 's/"\(aws-s3[-a-z]*\)"[ ]*{= .*}/"\1" {= "$(VERSION)" }/' *.opam

# release: VERSION=$(shell cat Changelog | grep -E '^[0-9]' | head -n 1 | cut -f1 -d':')
# release: update-version
# 	opam publish
#
#
# Before this run `dune-release tag <VERSION>`
# opam-release:
# 	dune-release distrib --skip-build --skip-lint --skip-tests
# 	dune-release publish distrib --verbose
# 	dune-release opam pkg
# 	dune-release opam submit
