# -*- coding: utf-8 -*-
#
# Configuration file for the Sphinx documentation builder.
#
# This file does only contain a selection of the most common options. For a
# full list see the documentation:
# http://www.sphinx-doc.org/en/master/config

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))


# -- Project information -----------------------------------------------------

project = 'Finkel'
copyright = '2019-2021, 8c6794b6'
author = '8c6794b6'

# The short X.Y version
version = ''
# The full version, including alpha/beta/rc tags
release = ''


# -- General configuration ---------------------------------------------------

# If your documentation needs a minimal Sphinx version, state it here.
#
# needs_sphinx = '1.0'

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# The suffix(es) of source filenames.
# You can specify multiple suffix as a list of string:
#
# source_suffix = ['.rst', '.md']
source_suffix = '.rst'

# The master toctree document.
master_doc = 'index'

# The language for content autogenerated by Sphinx. Refer to documentation
# for a list of supported languages.
#
# This is also used if you do content translation via gettext catalogs.
# Usually you set "language" from the command line for these cases.
language = None

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

# The name of the Pygments (syntax highlighting) style to use.
# pygments_style = None
# pygments_style = 'colorful'
# pygments_style = 'default'
# pygments_style = 'emacs'
pygments_style = 'friendly'


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'sphinx_rtd_theme'
# html_theme = 'alabaster'

# Theme options are theme-specific and customize the look and feel of a theme
# further.  For a list of options available for each theme, see the
# documentation.
#
html_theme_options = {}

# For 'alabaster' theme
# html_theme_options = {
#     'fixed_sidebar': True,
#     'show_relbars': True,
# }

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']

# Custom sidebar templates, must be a dictionary that maps document names
# to template names.
#
# The default sidebars (for documents that don't match any pattern) are
# defined by theme itself.  Builtin themes are using these templates by
# default: ``['localtoc.html', 'relations.html', 'sourcelink.html',
# 'searchbox.html']``.
#
# html_sidebars = {}


# -- Options for HTMLHelp output ---------------------------------------------

# Output file base name for HTML help builder.
htmlhelp_basename = 'Finkeldoc'


# -- Options for LaTeX output ------------------------------------------------

latex_elements = {
    # The paper size ('letterpaper' or 'a4paper').
    #
    # 'papersize': 'letterpaper',

    # The font size ('10pt', '11pt' or '12pt').
    #
    # 'pointsize': '10pt',

    # Additional stuff for the LaTeX preamble.
    #
    # 'preamble': '',

    # Latex figure (float) alignment
    #
    # 'figure_align': 'htbp',
}

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title,
#  author, documentclass [howto, manual, or own class]).
latex_documents = [
    (master_doc, 'Finkel.tex', 'Finkel Documentation',
     '8c6794b6', 'manual'),
]


# -- Options for manual page output ------------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    (master_doc, 'finkel', 'Finkel Documentation',
     [author], 1)
]


# -- Options for Texinfo output ----------------------------------------------

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
texinfo_documents = [
    (master_doc, 'Finkel', 'Finkel Documentation',
     author, 'Finkel', 'Lisp Flavored Haskell',
     'Miscellaneous'),
]


# -- Options for Epub output -------------------------------------------------

# Bibliographic Dublin Core info.
epub_title = project

# The unique identifier of the text. This can be a ISBN number
# or the project homepage.
#
# epub_identifier = ''

# A unique identification for the text.
#
# epub_uid = ''

# A list of files that should not be packed into the epub file.
epub_exclude_files = ['search.html']


# -- Custom setup function

from pygments.lexer import RegexLexer, bygroups
from pygments import token
from pygments.token import Text, Comment, Number, String, Keyword, \
    Name, Operator, Punctuation
from pygments import unistring as uni
from sphinx.highlighting import lexers

import re

class FinkelLexer(RegexLexer):
    name = 'finkel'
    reserved = (
        # Haskell 2010
        'case', 'class', 'data', 'default', 'deriving', 'do',
        'family', 'if', 'infix', 'infixl', 'infixr', 'instance', 'let',
        'newtype', 'type', 'where',
        '_', '=', '=>', '<-', '->', '::',

        # Finkel kernel special forms
        ':begin', ':eval-when-compile', ':quote', ':quasiquote',
        ':unquote', ':unquote-splice',

        # Finkel core
        'eval-when', 'macrolet',
    )

    ascii = ('NUL', 'SOH', '[SE]TX', 'EOT', 'ENQ', 'ACK',
             'BEL', 'BS', 'HT', 'LF', 'VT', 'FF', 'CR', 'S[OI]', 'DLE',
             'DC[1-4]', 'NAK', 'SYN', 'ETB', 'CAN',
             'EM', 'SUB', 'ESC', '[FGRU]S', 'SP', 'DEL')

    tokens = {
        'root': [
            # Whitespace:
            (r'\s+', Text),

            # Pragma:
            (r'%p\(.*', token.Comment),

            # Comment:
            (r'#\;', Comment.Multiline, 'multiline-comment'),
            (r';.*', Comment.Single),
            (r'%_', Comment.Single),

            # Numbers
            (r'-?\d+\.\d+', Number.Float),
            (r'-?\d(_*\d)*_*[eE][+-]?\d(_*\d)*', Number.Float),
            (r'0[oO]_*[0-7](_*[0-7])*', Number.Oct),
            (r'0[xX]_*[\da-fA-F](_*[\da-fA-F])*', Number.Hex),
            (r'-?\d+', Number.Integer),

            # Characters:
            (r"#'", String.Char, 'character'),

            # String literal
            (r'"', String, 'string'),

            # Core macros
            (r"(defn)(\s+\(?)(::)?(\s+)?([^\s]+)",
             bygroups(Keyword.Reserved, Text, Keyword.Reserved,
                      Text, Name.Function)),

            (r"(defmacro)(\s+)([^\s]+)",
             bygroups(Keyword.Reserved, Text, Name.Function)),

            (r"(defmodule)(\s+)([A-Z]\w+)",
             bygroups(Keyword.Reserved, Text, Name.Namespace),
             'defmodule'),

            # Module header
            (r"(module)(\s+)([^\s]+)",
             bygroups(Keyword.Reserved, Text, Name.Namespace)),

            # Macro specific keywords
            (r':compile', String),
            (r':load', String),

            # Keywords
            ('(%s)' % '|'.join(re.escape(e) + ' ' for e in reserved),
             Keyword.Reserved),

            # Import
            (r'(import|:require)(\s+)(qualified)?(\s+)?([A-Z][\w\.]+)(\s+)?(as|hiding)?',
             bygroups(Keyword.Reserved,
                      Text,
                      Keyword.Reserved,
                      Text,
                      Name.Namespace,
                      Text,
                      Keyword.Reserved),
             'funclist'),

            # Types
            (r'([A-Z][0-9a-zA-Z\-_]*)', Keyword.Type),

            # Operators
            (r'([!@$%^&*-=+?/<>\|~]+)', Operator),
            (r'(,@|,)', Operator),

            # Variable identifier
            (r'[_a-z][\w\']*', Name),

            # Lambda
            (r'\\', Keyword.Reserved),

            # Puctuation
            (r'(\(|\))', Punctuation),
            (r'(\[|\])', Punctuation),
            (r'(\{|\})', Punctuation),
            (r'`', Punctuation),
            (r"'", Punctuation),
        ],

        'character': [
            (r"[^\\]", String.Char, '#pop'),
            (r"\\", String.Escape, 'escape'),
            (r" ", String.Char, '#pop'),
        ],

        'string': [
            (r'[^\\"]+', String),
            (r"\\", String.Escape, 'escape'),
            ('"', String, '#pop'),
        ],

        'escape': [
            (r'[abfnrtv"\'&\\]', String.Escape, '#pop'),
            (r'\^[][' + uni.Lu + r'@^_]', String.Escape, '#pop'),
            ('|'.join(ascii), String.Escape, '#pop'),
            (r'o[0-7]+', String.Escape, '#pop'),
            (r'x[\da-fA-F]+', String.Escape, '#pop'),
            (r'\d+', String.Escape, '#pop'),
            (r'\s+\\', String.Escape, '#pop'),
            (r'', String.Escape, '#pop'),
        ],

        'defmodule': [
            (r'\s+', Text),
            (r'export', Keyword.Reserved, 'funclist'),
            (r'(import-when)(\s+)(\[)(:compile|:load)+(\])(\s+)',
             bygroups(Keyword.Reserved, Text, Punctuation,
                      String, Punctuation, Text),
             'import-body'),
            (r'import', Keyword.Reserved, 'import-body'),
            (r'require', Keyword.Reserved, 'import-body'),
            (r'\(', Punctuation, '#push'),
            (r'\)', Punctuation, '#pop'),
            (r'', Text, '#pop'),
        ],

        'import-body': [
            (r'\s+', Text),
            (r'(as|hiding)', Keyword.Reserved),
            (r'[A-Za-z_.]+', Name.Namespace, 'funclist'),
            (r'\(', Punctuation, '#push'),
            (r'\)', Punctuation, '#pop'),
            (r'', Text, '#pop'),
        ],

        'funclist': [
            (r'\s+', Text),
            (r'(_[\w\']+|[' + uni.Ll + r'][\w\'-]*)', Name.Function),
            (r'\(', Punctuation, '#push'),
            (r'\)', Punctuation, '#pop'),
            (r'', Text, '#pop'),
        ],

        'multiline-comment': [
            (r'#\;', Comment.Multiline, '#push'),
            (r'\;#', Comment.Multiline, '#pop'),
            (r'[^;#]+', Comment.Multiline),
            (r'[;#]', Comment.Multiline),
        ]
    }

def setup(sphinx):
    sphinx.add_lexer("finkel", FinkelLexer)
