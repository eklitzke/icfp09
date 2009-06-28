#!/usr/bin/env python

import pygtk
pygtk.require('2.0')

import gobject
import pango
import gtk
import math
import time
import simplejson
import random
from gtk import gdk
try:
    import cairo
except ImportError:
    pass

if gtk.pygtk_version < (2,3,93):
    print "PyGtk 2.3.93 or later required"
    raise SystemExit

TEXT = 'TeamCA'
BORDER_WIDTH = 10



class PyGtkWidget(gtk.Widget):
	__gsignals__ = { 'realize': 'override',
					 'expose-event' : 'override',
					 'size-allocate': 'override',
					 'size-request': 'override',}

	def __init__(self):
		gtk.Widget.__init__(self)
		self.draw_gc = None
		self.layout = self.create_pango_layout(TEXT)
		self.layout.set_font_description(pango.FontDescription("sans serif 8"))

		self.epoch = 0

		def progress_timeout(obj):
			x, y, w, h = obj.allocation
			obj.window.invalidate_rect((0,0,w,h),False)
			self.epoch += 1
			if (self.epoch >= len(self.json_data)):
				self.epoch = 0
			return True

		# Draw the simulation at 24 fps
		gobject.timeout_add(1000 / 24.0, progress_timeout, self)

		json_data = open('icfp.json').read()
		lines = [simplejson.loads(line) for line in json_data.split('\n') if line]
		self.universe_size = lines[0]['universe_size']
		print 'UNIVERSE SIZE = %1.3g' % self.universe_size
		self.json_data = lines[1:]
										   
	def do_realize(self):
		self.set_flags(self.flags() | gtk.REALIZED)
		self.window = gdk.Window(self.get_parent_window(),
								 width=self.allocation.width,
								 height=self.allocation.height,
								 window_type=gdk.WINDOW_CHILD,
								 wclass=gdk.INPUT_OUTPUT,
								 event_mask=self.get_events() | gdk.EXPOSURE_MASK)
		if not hasattr(self.window, "cairo_create"):
			self.draw_gc = gdk.GC(self.window,
								  line_width=5,
								  line_style=gdk.SOLID,
								  join_style=gdk.JOIN_ROUND)

		self.window.set_user_data(self)
		self.style.attach(self.window)
		self.style.set_background(self.window, gtk.STATE_NORMAL)
		self.window.move_resize(*self.allocation)

	def do_size_request(self, requisition):
		width, height = self.layout.get_size()
		requisition.width = (width // pango.SCALE + BORDER_WIDTH*4)* 1.45
		requisition.height = (3 * height // pango.SCALE + BORDER_WIDTH*4) * 1.2

	def do_size_allocate(self, allocation):
		self.allocation = allocation
		if self.flags() & gtk.REALIZED:
			self.window.move_resize(*allocation)

	def _expose_gdk(self, event):
		x, y, w, h = self.allocation
		self.layout = self.create_pango_layout('no cairo')
		fontw, fonth = self.layout.get_pixel_size()
		self.style.paint_layout(self.window, self.state, False,
								event.area, self, "label",
								(w - fontw) / 2, (h - fonth) / 2,
								self.layout)

	def _expose_cairo(self, event, cr):

		# The json file has the following structure. The first line in the file
		# is a special line that gives the size of the universe, etc. If the
		# universe size is N, that means that the universe should be rendered as
		# a square whose sides are at (-N, N). For instance, if the size is 100,
		# then the upper-right corner of the universe is at (100, 100), and the
		# lower left corner of the universe is at (-100, -100).
		#
		# Each following line in the file is a list of objects to draw, and the
		# line corresponds to a time epoch in the simulation. So if a simulation
		# took 100 time steps, the json file will have 101 lines -- 1 line for
		# the initialization, and 100 lines for each of the epochs.

		get_red = lambda x: x.get('r', random.random() / 2)
		get_blue = lambda x: x.get('b', random.random() / 2)
		get_green = lambda x: x.get('g', random.random() / 2)
		get_alpha = lambda x: x.get('alpha', 0.8)

		set_rgba = lambda obj: cr.set_source_rgba(get_red(obj), get_green(obj), get_blue(obj), get_alpha(obj))

		x, y, w, h = self.allocation
		dim = min(w, h)

		def scale(thing):
			"""Scale an object to the correct placement on the buffer. This
			scales by the dimensions of the actual GTK window, and also the
			dimensions of the universe.

			EXAMPLE: if the universe has size 1e6, and thing is 5e5, then the
			value returned should be such that the object is halfway between the
			center of the universe and the right edge. So if w = h = 100, then
			this should return 75. If thing was -5e5 instead, then this should
			return 25.
			"""

			# ratio is in the range [0, 1], with 0.5 in the center of the universe
			ratio = 0.5 * (1 + thing / float(self.universe_size))
			assert 0 <= ratio <= 1

			return dim * ratio

		for obj in self.json_data[self.epoch]:
			assert isinstance(obj, dict)
			# A circle has required attribs x, y, r

			x = scale(obj['x'])

			# translate y into cartesian coordinates
			y = dim - scale(obj['y'])

			if obj['shape'] == 'circle':
				assert (obj['x'], obj['y']) == (0, 0)
				radius = dim * obj['R'] / self.universe_size / 2
				set_rgba(obj)
				cr.arc(x, y, radius, 0, 2 * math.pi)

			# A point has required attribs x, y
			elif obj['shape'] == 'dot':
				set_rgba(obj)
				cr.arc(x, y, 2, 0, 2 * math.pi)
				cr.fill()

			#if obj['note'] == 'satellite':
			#	print 'for satellite, had pos = (%1.3g, %1.3g), chose x, y = %s' % (obj['x'], obj['y'], (x, y))
			#if obj['note'] == 'earth':
			#	print 'for earth, had R = %1.3g, chose radius = %s' % (obj['R'], radius)
			cr.stroke()
		
	def do_expose_event(self, event):
		self.chain(event)
		try:
			cr = self.window.cairo_create()
		except AttributeError:
			return self._expose_gdk(event)
		return self._expose_cairo(event, cr)


json_data = open('icfp.json').read()

win = gtk.Window()
win.set_title("icfp'09 sim")
win.connect('delete-event', gtk.main_quit)

event_box = gtk.EventBox()
event_box.connect("button_press_event", lambda w,e: win.set_decorated(not win.get_decorated()))

win.add(event_box)

w = PyGtkWidget()
event_box.add(w)

win.show_all()

gtk.main()
