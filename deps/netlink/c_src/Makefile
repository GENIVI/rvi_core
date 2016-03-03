.PHONY: libnl

PRIVDIR := ../priv

all : $(PRIVDIR)/netlink_drv.so

override CFLAGS += -fpic -shared 

$(PRIVDIR)/netlink_drv.so : netlink_drv.o
	$(CC) -shared -fpic $(LDFLAGS) -o $@ $^

clean:
	$(RM) -f $(PRIVDIR)/netlink_drv.so netlink_drv.o
