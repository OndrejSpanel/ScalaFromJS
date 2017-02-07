// from https://raw.githubusercontent.com/Skalman/UglifyJS-online/gh-pages/script.js

/*global defaults:false, parse:false, Compressor:false, JS_Parse_Error:false, DefaultsError:false */
/*jshint globalstrict:true */

'use strict';

// Create a simple wrapper around UglifyJS

var default_options = {};
function uglify(code, options) {
    // Create copies of the options
    var parse_options = defaults({}, options.parse);
    var compress_options = defaults({}, options.compress);
    var output_options = defaults({}, options.output);

    parse_options = defaults(parse_options, default_options.parse, true);
    compress_options = defaults(compress_options, default_options.compress, true);
    output_options = defaults(output_options, default_options.output, true);

    // 1. Parse
    var toplevel_ast = parse(code, parse_options);
    toplevel_ast.figure_out_scope();

    // 2. Compress
    var compressor = new Compressor(compress_options);
    var compressed_ast = toplevel_ast.transform(compressor);

    // 3. Mangle
    compressed_ast.figure_out_scope();
    compressed_ast.compute_char_frequency();
    compressed_ast.mangle_names();

    // 4. Generate output
    code = compressed_ast.print_to_string(output_options);

    return code;
}

function $(id) {
    return document.getElementById(id);
}

window.console = window.console || { log: function () {}, error: function () {} };
