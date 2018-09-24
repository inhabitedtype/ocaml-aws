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

aws-ec2:
	dune exec aws_gen -- --is-ec2 -i input/ec2/latest/service-2.json -r input/ec2/overrides.json -e input/errors.json -o libraries

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
	aws-sts

.PHONY: $(LIBRARIES)
$(LIBRARIES): aws-%:
	dune exec aws_gen -- -i input/$*/latest/service-2.json -r input/$*/overrides.json -e input/errors.json -o libraries

gen: build aws-ec2 $(LIBRARIES)

compile-libraries:
	for dir in libraries/*; \
		do dune build --root "$$dir/"; \
	done;

test-libraries:
	for dir in $LIBRARIES; \

		do dune runtest -f ; \
	done;
