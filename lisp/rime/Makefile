ifdef MODULE_FILE_SUFFIX
	SUFFIX = $(MODULE_FILE_SUFFIX)
else
	SUFFIX = .so
endif

TARGET = rime-module$(SUFFIX)

CC     = gcc

ifdef DEBUG
	CFLAGS = -fPIC -g -Wall
else
	CFLAGS = -fPIC -O2 -Wall
endif

ifeq ($(SUFFIX), .dylib)
	LDFLAGS = -dynamiclib
else
	LDFLAGS = -shared
endif

ifdef LIBRIME_ROOT
	CFLAGS  += -I ${LIBRIME_ROOT}include/
	LDFLAGS += -L ${LIBRIME_ROOT}lib/ -Wl,-rpath ${LIBRIME_ROOT}lib/ -lrime
else
	LDFLAGS += -lrime
endif

ifdef EMACS_MODULE_HEADER_ROOT
	CFLAGS += -I ${EMACS_MODULE_HEADER_ROOT}
endif

all: clean rime-module

clean:
	-rm -rf $(TARGET)

rime-module:
	$(CC) rime-module.c -o $(TARGET) $(CFLAGS) $(LDFLAGS)
