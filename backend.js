const pluginName = 'backend';
const modules = './src/modules/';

const kss = require('kss');
const R = require('ramda');
const Transform = require('stream').Transform;

function generate() {
	const transformStream = new Transform({objectMode: true});
	const pl = [];

	transformStream._transform = function (file, encoding, callback) {
		if (file.isNull()) {
			return callback(null, file);
		}

		if (file.isStream()) {
			this.emit('error', new PluginError(pluginName, 'Streams not supported!'));
		}

		if (!file.isBuffer()) {
			this.emit('error', new PluginError(pluginName, 'Only Buffer supported'));
		}

		const patternlab = kss.parse(file.toString('utf8'));

		pl.push(patternlab);
		return callback();
	};

	transformStream._flush = function(callback) {
		console.log(pl)
		callback();
	}

	return transformStream;
}

module.exports.generate = generate;


/**
 * TODO:
 * - refactor modules kss-parser, kss-splitter, kss-sanitize-params,
 * kss-additional-params, markdown, parsers/
 *
 */
