#ifndef CPP2SIR_REG_HPP
#define CPP2SIR_REG_HPP

#include "cfg.hpp"

#include <clang/AST/Decl.h>
#include <clang/AST/DeclCXX.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/StmtCXX.h>
#include <boost/assert.hpp>
#include <boost/variant.hpp>
#include <boost/utility.hpp>
#include <vector>
#include <list>
#include <map>
#include <cstdlib>

// TODO: put these to a more reasonable header
enum eop_type { eot_none, eot_func, eot_oper, eot_const, eot_member, eot_node, eot_var, eot_varptr, eot_nodetgt, eot_vartgt };

struct eop
{
	eop_type type;
	cfg::op_id id;

	eop(cfg::operand op)
		: type(static_cast<eop_type>(op.type)), id(op.id)
	{
	}

	eop(eop_type type = eot_none, cfg::op_id id = boost::none)
		: type(type), id(id)
	{
	}
};

struct enode
{
	cfg::node_type type;
	std::vector<eop> ops;
	clang::Stmt const * data;

	enode(cfg::node_type type, clang::Stmt const * data = 0)
		: type(type), data(data)
	{
	}

	enode & operator()(eop const & op)
	{
		ops.push_back(op);
		return *this;
	}

	enode & operator()(eop_type type, cfg::op_id id)
	{
		ops.push_back(eop(type, id));
		return *this;
	}
};

// There are two types of registration records---an except clause registration and
// the more common automatic variable registration.
//
// The former is used to introduce try-except statement into the block scope chain.
// These registrations are used during the final exception path generation crawl
// to direct exceptions to their respective handlers.
//
// The latter is used to register variables with automatic lifetime and
// non-trivial destructors so as to allow the CFG generator to emit a call to it
// when a scope (either a full expression scope or a block scope) is left.
// This includes break, continue and return statements, throw statements and exception forwarding.

struct except_regrec
{
	cfg::vertex_descriptor entry_node;
};

struct var_regrec
{
	clang::CXXDestructorDecl const * destr;
	eop varptr;
};

typedef boost::variant<boost::none_t, var_regrec, except_regrec> regrec;

// A registration context is a chain of registration records. As the AST of a function body
// is crawled and new declarations and try statements are encountered, the registration 
// context changes.

class context_registry
{
public:
	typedef std::list<std::size_t>::iterator node_descriptor;
	typedef std::size_t context_type;

	context_registry()
		: m_nodes(1), m_context(1)
	{
	}

	node_descriptor add(regrec const & r)
	{
		BOOST_ASSERT(!m_context.empty());

		node n = { r, m_context.back() };
		m_nodes.push_back(n);
		m_context.push_back(m_nodes.size() - 1);

		return boost::prior(m_context.end());
	}

	void remove(node_descriptor nd)
	{
		// TODO: exception guarantees

		std::size_t parent = m_nodes[*nd].parent;
		nd = m_context.erase(nd);

		for (; nd != m_context.end(); ++nd)
		{
			node n = { m_nodes[*nd].rec, parent };
			m_nodes.push_back(n);
			*nd = m_nodes.size() - 1;
			parent = *nd;
		}
	}

	context_type empty_context() const
	{
		return 0;
	}

	context_type current_context() const
	{
		return m_context.back();
	}

	context_type parent(context_type ctx) const
	{
		// FIXME: assert
		return m_nodes[ctx].parent;
	}

	context_type begin() const { return 0; }
	context_type end() const { return m_nodes.size(); }

	context_type context(node_descriptor n) const
	{
		return *n;
	}

	template <typename Visitor>
	void visit_all(Visitor & visitor)
	{
		for (std::size_t ctx = m_nodes.size() - 1; ctx != 0; --ctx)
			m_nodes[ctx].rec.apply_visitor(visitor);
	}

	regrec const & value(context_type ctx) const
	{
		return m_nodes[ctx].rec;
	}

	template <typename Visitor>
	void visit_range(context_type ctx, context_type ancestor, Visitor & visitor)
	{
		BOOST_ASSERT(ctx > 0 && ctx < m_nodes.size());

		for (; ctx != ancestor; ctx = m_nodes[ctx].parent)
		{
			BOOST_ASSERT(ctx != 0);
			m_nodes[ctx].rec.apply_visitor(visitor);
		}
	}

	template <typename T>
	T get(context_type ctx) const
	{
		return boost::get<T>(m_nodes[ctx].rec);
	}

private:
	struct node
	{
		regrec rec;
		std::size_t parent;
	};

	std::vector<node> m_nodes;
	std::list<std::size_t> m_context;
};

// A jump registration records a request for an out-of-flow control redirection.
// Break, continue, goto, return and throw statements generate jump registrations.

struct jump_sentinel
{
	jump_sentinel()
	{
	}

	jump_sentinel(cfg::vertex_descriptor sentinel, eop const & value = eop())
		: sentinel(sentinel), value(value)
	{
	}

	cfg::vertex_descriptor sentinel;
	eop value;
};

typedef std::map<context_registry::context_type, std::vector<jump_sentinel> > jump_registry;

#endif
