import simplejson


if __name__ == '__main__':
	outfile = open('icfp.json', 'w')

	inited = False

	for line in open('hs.json'):
		data = simplejson.loads(line)
		if not inited:
			outfile.write(simplejson.dumps({'universe_size': data['oRadius'] * 2}) + '\n')
			inited = True

		output = []

		# Draw the target radius as a green circle
		output.append({'shape': 'circle', 'R': data['oRadius'], 'x': 0, 'y': 0, 'r': 0, 'b': 0, 'g': 1})

		# Draw the satellite as a red dot
		output.append({'shape': 'dot', 'x': data['oPos'][0], 'y': data['oPos'][1], 'r': 1, 'b': 0, 'g': 0})

		# Draw the earth as a blue dot at (0, 0)
		output.append({'shape': 'circle', 'R': 6.357e6, 'x': 0, 'y': 0, 'b': 1, 'r': 0, 'g': 0})
		outfile.write(simplejson.dumps(output) + '\n')
