#include <X11/Xlib.h> 
#include <unistd.h>

#define NIL (0)

static const char text[] = "hello world!";

main()
{
      // Open the display
      Display *dpy = XOpenDisplay(NIL);

      int blackColor = BlackPixel(dpy, DefaultScreen(dpy));
      int whiteColor = WhitePixel(dpy, DefaultScreen(dpy));

      // Create the window
      Window w = XCreateSimpleWindow(dpy, DefaultRootWindow(dpy), 0, 
            0, 200, 100, 0, blackColor, blackColor);

      // Register MapNotify events
      XSelectInput(dpy, w, StructureNotifyMask);

      // Make the window appear on the screen
      XMapWindow(dpy, w);

      // Create a new graphics context
      GC gc = XCreateGC(dpy, w, 0, NIL);

      XSetForeground(dpy, gc, whiteColor);

      // Loop until we get a MapNotify event
      for(;;) {
          XEvent e;
          XNextEvent(dpy, &e);
          if (e.type == MapNotify)
              break;
      }

      XFontStruct *fs = XLoadQueryFont(dpy, "cursor");

      XDrawString(dpy, w, gc, 40, 50, text, sizeof(text));

      // Flush the commands to the X server
      XFlush(dpy);

      sleep(8);
}
