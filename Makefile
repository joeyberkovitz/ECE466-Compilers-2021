BUILD_DIR ?= ./build
SRC_DIRS ?= ./src

SRCS := $(shell find $(SRC_DIRS) -name "*.y" -or -name "*.l" -or -name "*.c")
OBJS := $(SRCS:%=$(BUILD_DIR)/%.o)
DEPS := $(OBJS:.o=.d)

INC_DIRS := $(shell find $(SRC_DIRS) -type d)
INC_DIRS += $(addprefix $(BUILD_DIR)/,$(INC_DIRS))
INC_FLAGS := $(addprefix -I,$(INC_DIRS))

CFLAGS ?= $(INC_FLAGS) -MMD -MP -std=gnu11 -g -fms-extensions
CC = gcc
LEX = flex
BISON = bison

all: lexer parser

lexer: $(OBJS) lextest.c
	$(CC) lextest.c $(CFLAGS) $(OBJS) -o build/$@ $(LDFLAGS)

parser: $(OBJS) parsertest.c
	$(CC) parsertest.c $(CFLAGS) $(OBJS) -o build/$@ $(LDFLAGS)

# bison
$(BUILD_DIR)/%.y.o: %.y
	$(MKDIR_P) $(dir $@)
	$(BISON) -v -o $@.c --defines=$@.h $<
	$(CC) $(CFLAGS) -c $@.c -o $@

# flex
$(BUILD_DIR)/%.l.o: %.l $(BUILD_DIR)/$(SRC_DIRS)/parser/parser.y.o
	$(MKDIR_P) $(dir $@)
	$(LEX) -o $@.c --header-file=$@.h $<
	$(CC) $(CFLAGS) -c $@.c -o $@

# c source
$(BUILD_DIR)/%.c.o: %.c $(BUILD_DIR)/$(SRC_DIRS)/lexer/lexer.l.o
	$(MKDIR_P) $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

.PHONY: clean

clean:
	$(RM) -r $(BUILD_DIR)

-include $(DEPS)

MKDIR_P ?= mkdir -p

