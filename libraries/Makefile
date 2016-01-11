.PHONY: build doc all install uninstall reinstall clean distclean configure

build:
	for lib in $$(ls); do \
		if [ -d "$$lib" ]; then \
			$(MAKE) -C $$lib build; \
		fi; \
	done

doc:
	for lib in $$(ls); do \
		if [ -d "$$lib" ]; then \
			$(MAKE) -C $$lib doc; \
		fi; \
	done

test:
	for lib in $$(ls); do \
		if [ -d "$$lib" ]; then \
			$(MAKE) -C $$lib test; \
		fi; \
	done

all:
	for lib in $$(ls); do \
		if [ -d "$$lib" ]; then \
			$(MAKE) -C $$lib all; \
		fi; \
	done

install:
	for lib in $$(ls); do \
		if [ -d "$$lib" ]; then \
			$(MAKE) -C $$lib install; \
		fi; \
	done

uninstall:
	for lib in $$(ls); do \
		if [ -d "$$lib" ]; then \
			$(MAKE) -C $$lib uninstall; \
		fi; \
	done

reinstall:
	for lib in $$(ls); do \
		if [ -d "$$lib" ]; then \
			$(MAKE) -C $$lib reinstall; \
		fi; \
	done

clean:
	for lib in $$(ls); do \
		if [ -d "$$lib" ]; then \
			$(MAKE) -C $$lib clean; \
		fi; \
	done

distclean:
	for lib in $$(ls); do \
		if [ -d "$$lib" ]; then \
			$(MAKE) -C $$lib distclean; \
		fi; \
	done

configure:
	for lib in $$(ls); do \
		if [ -d "$$lib" ]; then \
			$(MAKE) -C $$lib configure; \
		fi; \
	done
