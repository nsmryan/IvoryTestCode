ARDUINO_DIR = /usr/share/arduino
BOARD_TAG = uno
ARDUINO_PORT = /dev/ttyACM0
ARD_PORT = /dev/ttyACM*
ARDUINO_LIBS = EEPROM TimerOne
include /usr/share/arduino/Arduino.mk
TARGET = IvoryTest

AVR_TOOLS_PATH = /usr/bin
AVRDUDE_CONF = /etc/avrdude.conf
