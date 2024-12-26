CC = gcc
CFLAGS_DEBUG = -Wall -g
CFLAGS_RELEASE = -Wall -O3

SRC_DIR = src
SRC = $(wildcard $(SRC_DIR)/*.c)
OBJ_DEBUG = $(patsubst $(SRC_DIR)/%.c, bin-int/debug-x64/%.o, $(SRC))
OBJ_RELEASE = $(patsubst $(SRC_DIR)/%.c, bin-int/release-x64/%.o, $(SRC))

BIN_INT_DIR_DEBUG = bin-int/debug-x64
BIN_INT_DIR_RELEASE = bin-int/release-x64
BIN_DIR_DEBUG = bin/debug-x64
BIN_DIR_RELEASE = bin/release-x64

TARGET_DEBUG = $(BIN_DIR_DEBUG)/my_program_debug.exe
TARGET_RELEASE = $(BIN_DIR_RELEASE)/my_program_release.exe

OBJ_DEBUG = $(BIN_INT_DIR_DEBUG)/main.o
OBJ_RELEASE = $(BIN_INT_DIR_RELEASE)/main.o

all: debug release

$(BIN_INT_DIR_DEBUG) $(BIN_DIR_DEBUG):
	mkdir -p $(BIN_INT_DIR_DEBUG) $(BIN_DIR_DEBUG)

debug: $(BIN_INT_DIR_DEBUG) $(TARGET_DEBUG)

$(TARGET_DEBUG): $(OBJ_DEBUG)
	$(CC) $(CFLAGS_DEBUG) -o $@ $^

$(OBJ_DEBUG): $(SRC) | $(BIN_INT_DIR_DEBUG)
	$(CC) $(CFLAGS_DEBUG) -c $(SRC) -o $@


$(BIN_INT_DIR_RELEASE) $(BIN_DIR_RELEASE):
	mkdir -p $(BIN_INT_DIR_RELEASE) $(BIN_DIR_RELEASE)

release: $(BIN_INT_DIR_RELEASE) $(TARGET_RELEASE)

$(TARGET_RELEASE): $(OBJ_RELEASE)
	$(CC) $(CFLAGS_RELEASE) -o $@ $^

$(OBJ_RELEASE): $(SRC) | $(BIN_INT_DIR_RELEASE)
	$(CC) $(CFLAGS_RELEASE) -c $(SRC) -o $@

clean:
	rm -rf bin-int
	rm -rf bin