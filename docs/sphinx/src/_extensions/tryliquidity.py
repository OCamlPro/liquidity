from __future__ import absolute_import
from docutils import nodes
import jinja2

from sphinx.util.docutils import SphinxDirective
from sphinx.directives.code import LiteralIncludeReader


import urllib

BUTTON_TEMPLATE = jinja2.Template(u"""
<a href="{{ link }}" target="_blank"
   class="btn btn-small btn-info">
   {{ text }} <span class="fa fa-tint"></span>
</a>
""")

# placeholder node for document graph
class button_node(nodes.General, nodes.Element):
    pass

class TryLiquidity(SphinxDirective):
# class TryLiquidity(LiteralInclude):
    required_arguments = 1

    # this will execute when your directive is encountered
    # it will insert a button_node into the document that will
    # get visisted during the build phase
    def run(self):
        location = self.state_machine.get_source_and_line(self.lineno)
        rel_filename, filename = self.env.relfn2path(self.arguments[0])
        self.env.note_dependency(rel_filename)

        reader = LiteralIncludeReader(filename, self.options, self.config)
        text, lines = reader.read(location=location)

        node = button_node()
        node['text'] = "Try online"
        node['link'] = "http://liquidity-lang.org/edit?source="+urllib.parse.quote_plus(text)
        return [node]

# build phase visitor emits HTML to append to output
def html_visit_button_node(self, node):
    html = BUTTON_TEMPLATE.render(text=node['text'], link=node['link'])
    self.body.append(html)
    raise nodes.SkipNode

def ignore(self, node):
    raise nodes.SkipNode

# if you want to be pedantic, define text, latex, manpage visitors too..

def setup(app):
    app.add_node(button_node,
                 html=(html_visit_button_node, None),
                 latex=(ignore, None),
                 expub=(ignore, None))
    app.add_directive('tryliquidity', TryLiquidity)
