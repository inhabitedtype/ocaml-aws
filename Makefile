.PHONY: all build clean test

build:
	jbuilder build --dev @install

all: build

test:
	jbuilder runtest --dev

install:
	jbuilder install --dev

uninstall:
	jbuilder uninstall

clean:
	rm -rf _build *.install

aws-ec2:
	jbuilder exec aws_gen -- --is-ec2 -i input/ec2/latest/service-2.json -r input/ec2/overrides.json -e input/errors.json -o libraries

# NOTE: This does not include aws-ec2, which is special-cased.
LIBRARIES := \
	aws-autoscaling \
	aws-cloudformation \
	aws-cloudtrail \
	aws-elasticache \
	aws-elasticloadbalancing \
	aws-rds \
	aws-sdb \
	aws-ssm \
	aws-sts \

.PHONY: $(LIBRARIES)
$(LIBRARIES): aws-%:
	jbuilder exec aws_gen -- -i input/$*/latest/service-2.json -r input/$*/overrides.json -e input/errors.json -o libraries

gen: all aws-ec2 $(LIBRARIES)
