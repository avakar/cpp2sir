#include "cfg_builder.hpp"
#include "reg.hpp"

#include <boost/assert.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/utility.hpp>
#include <boost/ref.hpp>

#include <clang/AST/CharUnits.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/StmtCXX.h>
#include <clang/AST/Expr.h>
#include <clang/AST/ExprCXX.h>
#include <clang/AST/Decl.h>
#include <clang/AST/DeclCXX.h>
#include <clang/AST/DeclTemplate.h>
#include <clang/Basic/SourceManager.h>
#include <clang/Basic/FileManager.h>

#include <list>

namespace {

// FIXME: Assumes that sizeof(wchar_t) == 2
std::vector<sir_int_t> string_literal_to_value_array(clang::StringLiteral const * sl)
{
	std::vector<sir_int_t> res;

	llvm::StringRef str = sl->getString();
	if (sl->isWide())
	{
		for (std::size_t i = 0; i < str.size(); i += 2)
			res.push_back(str[i+1] * 256 + (unsigned char)str[i]);
	}
	else
	{
		for (std::size_t i = 0; i < str.size(); ++i)
			res.push_back(str[i]);
	}

	res.push_back(0);
	return res;
}

struct context
{
	context(program & p, cfg & c, name_mangler & nm, clang::FunctionDecl const * fn, clang::SourceManager const & sm,
		filename_store & fnames, detail::build_cfg_visitor_base & visitor, std::string const & static_prefix)
		: m_program(p), m_name_mangler(nm), m_static_prefix(static_prefix), m_sm(sm), m_fnames(fnames), m_visitor(visitor), g(c), m_fn(fn),
		m_head(add_vertex(g))
	{
		g.entry(m_head);

		this->build();
	}

	program & m_program;
	name_mangler & m_name_mangler;
	std::string m_static_prefix;
	clang::SourceManager const & m_sm;
	filename_store & m_fnames;
	detail::build_cfg_visitor_base & m_visitor;

	void register_decl_ref(clang::FunctionDecl const * fn)
	{
		// TODO: Will we ever need this function?
	}

	cfg & g;
	clang::FunctionDecl const * m_fn;

	context_registry m_context_registry;
	typedef context_registry::node_descriptor context_node;
	typedef context_registry::context_type execution_context;
	typedef std::vector<context_node> lifetime_context_t;

	std::vector<lifetime_context_t> m_fullexpr_lifetimes;
	std::vector<lifetime_context_t> m_block_lifetimes;
	std::vector<clang::Type const *> m_temporaries;

	cfg::vertex_descriptor m_head;

	jump_registry m_exc_registry;
	jump_registry m_return_registry;
	std::vector<jump_registry> m_break_registries;
	std::vector<jump_registry> m_continue_registries;

	std::map<clang::GotoStmt const *, cfg::vertex_descriptor> m_gotos;
	std::map<clang::LabelStmt const *, cfg::vertex_descriptor> m_labels;

	typedef std::pair<cfg::vertex_descriptor, std::map<sir_int_t, cfg::vertex_descriptor> > case_context_t;
	std::vector<case_context_t> m_case_contexts;

	std::map<clang::NamedDecl const *, std::string> m_registered_names;

	eop join_jump_registry(std::vector<jump_registry> & registries, execution_context ec, cfg::vertex_descriptor target)
	{
		BOOST_ASSERT(!registries.empty());
		jump_sentinel s = this->generate_return_paths(registries.back(), ec);
		this->join_nodes(s.sentinel, target);
		registries.pop_back();
		return s.value;
	}

	void connect_to_exc(cfg::vertex_descriptor v, execution_context ctx)
	{
		cfg::vertex_descriptor sentinel = add_vertex(g);
		cfg::edge_descriptor e = add_edge(v, sentinel, g).first;
		g[e].id = 1;
		m_exc_registry[ctx].push_back(sentinel);
	}

	void connect_to_exc(cfg::vertex_descriptor v)
	{
		this->connect_to_exc(v, m_context_registry.current_context());
	}

	cfg::vertex_descriptor duplicate_vertex(cfg::vertex_descriptor src)
	{
		BOOST_ASSERT(out_degree(src, g) == 0);

		cfg::vertex_descriptor res = add_vertex(g);
		g[res] = g[src];

		std::pair<cfg::in_edge_iterator, cfg::in_edge_iterator> in_edge_range = in_edges(src, g);
		for (; in_edge_range.first != in_edge_range.second; ++in_edge_range.first)
		{
			cfg::edge_descriptor e = add_edge(source(*in_edge_range.first, g), res, g).first;
			g[e] = g[*in_edge_range.first];
		}
		return res;
	}

	void join_nodes(cfg::vertex_descriptor src, cfg::vertex_descriptor dest)
	{
		if (src != g.null_vertex())
		{
			g.redirect_vertex(src, dest);
			remove_vertex(src, g);
		}
	}

	std::string get_name(clang::NamedDecl const * decl) const
	{
		std::map<clang::NamedDecl const *, std::string>::const_iterator it = m_registered_names.find(decl);
		if (it == m_registered_names.end())
			return m_name_mangler.make_decl_name(decl, m_static_prefix);
		else
			return it->second;
	}

	void register_locals(clang::FunctionDecl const * fn)
	{
		if (fn->getResultType()->isStructureOrClassType())
			g.add_param("p:return");

		if (clang::CXXMethodDecl const * d = llvm::dyn_cast<clang::CXXMethodDecl>(fn))
		{
			if (!d->isStatic())
				g.add_param("p:this");
		}

		for (clang::FunctionDecl::param_const_iterator it = fn->param_begin(); it != fn->param_end(); ++it)
		{
			clang::ParmVarDecl const * d = *it;

			std::string name = "p:" + d->getQualifiedNameAsString();
			m_registered_names[d] = name;
			g.add_param(name);
		}

		std::set<std::string> used_names;
		for (clang::FunctionDecl::decl_iterator it = fn->decls_begin(); it != fn->decls_end(); ++it)
		{
			clang::Decl const * decl = *it;
			if (clang::VarDecl const * d = llvm::dyn_cast<clang::VarDecl>(decl))
			{
				std::string name_base;
				if (d->isStaticLocal() || decl->getKind() == clang::Decl::ParmVar)
					continue;

				name_base = "l:" + d->getQualifiedNameAsString();
				name_base += ':';

				std::string name;
				for (std::size_t i = 0;; ++i)
				{
					std::ostringstream ss;
					ss << name_base << i;
					if (used_names.find(ss.str()) == used_names.end())
					{
						name = ss.str();
						used_names.insert(name);
						m_registered_names[d] = name;
						break;
					}
				}

				g.add_local(name);
			}
		}
	}

	eop make_temporary(clang::Type const * type)
	{
		std::ostringstream ss;
		ss << "t:" << m_temporaries.size();

		m_temporaries.push_back(type);
		g.add_local(ss.str());
		return eop(eot_var, ss.str());
	}

	cfg::operand make_rvalue(eop const & op)
	{
		BOOST_ASSERT(op.type < eot_nodetgt);
		return cfg::operand(static_cast<cfg::op_type>(op.type), op.id);
	}

	cfg::operand make_rvalue(cfg::vertex_descriptor & head, eop const & op)
	{
		if (op.type == eot_nodetgt)
		{
			cfg::vertex_descriptor res = head;
			this->add_node(head, enode(cfg::nt_deref)
				(eot_node, op.id));
			return cfg::operand(cfg::ot_node, res);
		}
		else if (op.type == eot_vartgt)
		{
			cfg::vertex_descriptor res = head;
			this->add_node(head, enode(cfg::nt_deref)
				(eot_var, op.id));
			return cfg::operand(cfg::ot_node, res);
		}

		return this->make_rvalue(op);
	}

	eop make_param(cfg::vertex_descriptor & head, eop const & op, clang::Type const * type)
	{
		if (type->isReferenceType() && !this->is_lvalue(op))
		{
			eop temp = this->make_temporary(type);
			this->add_node(head, enode(cfg::nt_assign)
				(this->make_address(temp))
				(op));
			return this->make_address(temp);
		}

		if (type->isReferenceType() || type->isStructureOrClassType())
			return this->make_address(op);
		else
			return op;
	}

	template <typename ParamIter, typename ArgIter>
	void append_args(cfg::vertex_descriptor & head, enode & node, ParamIter param_first, ParamIter param_last, ArgIter arg_first, ArgIter arg_last)
	{
		for (; param_first != param_last; ++param_first, ++arg_first)
			node(this->make_param(head, this->build_expr(head, *arg_first), (*param_first)->getType().getTypePtr()));
		BOOST_ASSERT(arg_first == arg_last);
	}

	bool is_lvalue(eop const & op)
	{
		return op.type == eot_func || op.type == eot_nodetgt || op.type == eot_vartgt || op.type == eot_var;
	}

	eop make_address(eop op)
	{
		switch (op.type)
		{
		case eot_func:
			break;
		case eot_nodetgt:
			op.type = eot_node;
			break;
		case eot_vartgt:
			op.type = eot_var;
			break;
		case eot_var:
			op.type = eot_varptr;
			break;
		default:
			BOOST_ASSERT(0 && "lvalue is required to form an address");
		}
		return op;
	}

	eop make_deref(cfg::vertex_descriptor & head, eop op)
	{
		switch (op.type)
		{
		case eot_func:
			break;
		case eot_node:
			op.type = eot_nodetgt;
			break;
		case eot_nodetgt:
			op = this->make_rvalue(head, op);
			BOOST_ASSERT(op.type == eot_node);
			op.type = eot_nodetgt;
			break;
		case eot_varptr:
			op.type = eot_var;
			break;
		case eot_var:
			op.type = eot_vartgt;
			break;
		case eot_vartgt:
			op = this->make_rvalue(head, op);
			BOOST_ASSERT(op.type == eot_node);
			op.type = eot_nodetgt;
			break;
		default:
			BOOST_ASSERT(0 && "lvalue is required to form an address");
		}
		return op;
	}

	/**
	 * \brief Convert an eop to an op of type ot_node and of the same value.
	 */
	cfg::vertex_descriptor make_node(cfg::vertex_descriptor & head, eop op)
	{
		op = this->make_rvalue(head, op);
		if (op.type != eot_node)
			return this->add_node(head, enode(cfg::nt_value)(op));
		return boost::get<cfg::vertex_descriptor>(op.id);
	}

	cfg::vertex_descriptor make_cond_node(cfg::vertex_descriptor & head, eop op)
	{
		op = this->make_rvalue(head, op);
		if (op.type != eot_node || in_degree(head, g) != 1)
			return this->add_node(head, enode(cfg::nt_value)(op));

		cfg::vertex_descriptor res = boost::get<cfg::vertex_descriptor>(op.id);
		if (source(*in_edges(head, g).first, g) != res)
			return this->add_node(head, enode(cfg::nt_value)(op));

		return res;
	}

	void attach_range_tag(cfg::node & n, clang::SourceRange const & sr)
	{
		range_tag r = {};

		clang::PresumedLoc pl = m_sm.getPresumedLoc(sr.getBegin());
		r.fname = m_fnames.add(pl.getFilename());
		r.start_line = pl.getLine();
		r.start_col = pl.getColumn();
		pl = m_sm.getPresumedLoc(sr.getEnd());
		r.end_line = pl.getLine();
		r.end_col = pl.getColumn();

		n.tags.insert(g.add_tag(r));
	}

	cfg::node convert_node(cfg::vertex_descriptor & head, enode const & node)
	{
		cfg::node n;
		n.type = node.type;
		for (std::size_t i = 0; i < node.ops.size(); ++i)
			n.ops.push_back(this->make_rvalue(head, node.ops[i]));

		if (node.data)
			this->attach_range_tag(n, node.data->getSourceRange());
		return n;
	}

	cfg::vertex_descriptor add_node(cfg::vertex_descriptor & head, enode const & node)
	{
		BOOST_ASSERT(g[head].type == cfg::nt_none);
		BOOST_ASSERT(g[head].ops.empty());

		cfg::node n = this->convert_node(head, node);
		g[head] = n;

		cfg::vertex_descriptor new_head = add_vertex(g);
		add_edge(head, new_head, g);
		
		using std::swap;
		swap(head, new_head);
		return new_head;
	}

	void set_cond(cfg::vertex_descriptor node, std::size_t index, constant const & cond)
	{
		BOOST_ASSERT(in_degree(node, g) == 1);
		cfg::edge_descriptor edge = *in_edges(node, g).first;
		g[edge].id = index;
		g[edge].cond = cond;
	}

	void cleanup_lifetime_node(cfg::vertex_descriptor & head, context_node n)
	{
		execution_context ec = m_context_registry.context(n);
		m_context_registry.remove(n);

		return_path_generator g(*this, m_context_registry.current_context(), head);
		head = m_context_registry.value(ec).apply_visitor(g).sentinel;
	}

	void begin_lifetime_context(std::vector<lifetime_context_t> & ctx)
	{
		ctx.push_back(lifetime_context_t());
	}

	void end_lifetime_context(cfg::vertex_descriptor & head, std::vector<lifetime_context_t> & ctx)
	{
		BOOST_ASSERT(!ctx.empty());
		for (std::size_t i = ctx.back().size(); i != 0; --i)
			this->cleanup_lifetime_node(head, ctx.back()[i-1]);
		ctx.pop_back();
	}

	context_node register_destructible_var(clang::CXXDestructorDecl const * destructor, eop const & varptr)
	{
		var_regrec reg = { destructor, varptr };
		return m_context_registry.add(reg);
	}

	void build_construct_expr(cfg::vertex_descriptor & head, eop const & varptr, clang::CXXConstructExpr const * e)
	{
		BOOST_ASSERT(varptr.type != eot_none);

		enode node(cfg::nt_call, e);
		this->register_decl_ref(e->getConstructor());
		node(eot_func, this->get_name(e->getConstructor()));

		clang::FunctionProtoType const * fntype = llvm::dyn_cast<clang::FunctionProtoType>(e->getConstructor()->getType().getTypePtr());
		node(varptr);

		for (std::size_t i = 0; i < e->getNumArgs(); ++i)
			node(this->make_param(head, this->build_expr(head, e->getArg(i)), fntype->getArgType(i).getTypePtr()));

		cfg::vertex_descriptor call_node = this->add_node(head, node);

		if (!llvm::cast<clang::FunctionProtoType>(e->getConstructor()->getType()->getUnqualifiedDesugaredType())->hasEmptyExceptionSpec())
			this->connect_to_exc(call_node);
	}

	eop make_phi(cfg::vertex_descriptor & head, cfg::vertex_descriptor branch, eop headop, eop branchop, bool lvalue, clang::Expr const * data = 0)
	{
		if (lvalue)
		{
			headop = this->make_address(headop);
			branchop = this->make_address(branchop);
		}

		cfg::vertex_descriptor head_value_node = this->make_node(head, headop);
		cfg::vertex_descriptor branch_value_node = this->make_node(branch, branchop);
		this->join_nodes(branch, head);

		cfg::vertex_descriptor res_node = this->add_node(head, enode(cfg::nt_phi, data)
			(eot_node, head_value_node)
			(eot_node, branch_value_node));

		if (lvalue)
			return eop(eot_nodetgt, res_node);
		else
			return eop(eot_node, res_node);
	}

	eop build_expr(cfg::vertex_descriptor & head, clang::Expr const * expr)
	{
		BOOST_ASSERT(expr != 0);

		if (clang::BinaryOperator const * e = llvm::dyn_cast<clang::BinaryOperator>(expr))
		{
			// These should only appear as a part of CallExpr and should be handler right there.
			BOOST_ASSERT(e->getOpcode() != clang::BO_PtrMemD && e->getOpcode() != clang::BO_PtrMemI);

			eop const & lhs = this->build_expr(head, e->getLHS());

			if (e->getOpcode() == clang::BO_Assign)
			{
				// Treat assignment specially (takes a pointer to the assignee).
				eop const & rhs = this->build_expr(head, e->getRHS());
				this->add_node(head, enode(cfg::nt_assign, expr)
					(this->make_address(lhs))
					(rhs));
				return lhs;
			}
			else if (e->isCompoundAssignmentOp())
			{
				// Break compound assignments into separate instructions,
				// i.e. model a += b as an add followed by assign.
				eop const & rhs = this->build_expr(head, e->getRHS());

				cfg::node_type type;
				switch (e->getOpcode())
				{
				case clang::BO_MulAssign: type = cfg::nt_mul; break;
				case clang::BO_DivAssign: type = cfg::nt_div; break;
				case clang::BO_RemAssign: type = cfg::nt_rem; break;
				case clang::BO_AddAssign: type = cfg::nt_add; break;
				case clang::BO_SubAssign: type = cfg::nt_sub; break;
				case clang::BO_ShlAssign: type = cfg::nt_shl; break;
				case clang::BO_ShrAssign: type = cfg::nt_shr; break;
				case clang::BO_AndAssign: type = cfg::nt_and; break;
				case clang::BO_XorAssign: type = cfg::nt_xor; break;
				case clang::BO_OrAssign:  type = cfg::nt_or; break;
				default:
					BOOST_ASSERT(0 && "unknown compound assignment encountered");
				}

				cfg::vertex_descriptor v = this->add_node(head, enode(type, expr)(lhs)(rhs));
				this->add_node(head, enode(cfg::nt_assign, expr)(this->make_address(lhs))(eot_node, v));
				return lhs;
			}
			else if (e->getOpcode() == clang::BO_Comma)
			{
				this->build_expr(head, e->getLHS());
				return this->build_expr(head, e->getRHS());
			}
			else if (e->getOpcode() == clang::BO_LOr || e->getOpcode() == clang::BO_LAnd)
			{
				// These two operators are shot-circuiting, i.e. the right-hand side
				// operand is not evaluated, unless the result of the operation is still unknown 
				// even after the left-hand side evaluation is finished.

				cfg::vertex_descriptor cond_node = this->make_node(head, lhs);
				cfg::vertex_descriptor cont_head = this->duplicate_vertex(head);

				if (e->getOpcode() == clang::BO_LAnd)
					g[*in_edges(head, g).first].cond = sir_int_t(0);
				else
					g[*in_edges(cont_head, g).first].cond = sir_int_t(0);

				eop const & rhs = this->build_expr(cont_head, e->getRHS());
				cfg::vertex_descriptor rhs_value_node = this->make_node(cont_head, rhs);
				this->join_nodes(cont_head, head);
				return eop(eot_node, this->add_node(head, enode(cfg::nt_phi, e)
					(eot_node, rhs_value_node)
					(eot_node, cond_node)));
			}
			else
			{
				eop const & rhs = this->build_expr(head, e->getRHS());
				bool negate = false;

				cfg::node_type type;
				switch (e->getOpcode())
				{
				case clang::BO_Mul: type = cfg::nt_mul; break;
				case clang::BO_Div: type = cfg::nt_div; break;
				case clang::BO_Rem: type = cfg::nt_rem; break;
				case clang::BO_Add: type = cfg::nt_add; break;
				case clang::BO_Sub: type = cfg::nt_sub; break;
				case clang::BO_Shl: type = cfg::nt_shl; break;
				case clang::BO_Shr: type = cfg::nt_shr; break;
				case clang::BO_And: type = cfg::nt_and; break;
				case clang::BO_Xor: type = cfg::nt_xor; break;
				case clang::BO_Or:  type = cfg::nt_or; break;

				case clang::BO_EQ: type = cfg::nt_eq; break;
				case clang::BO_NE: type = cfg::nt_eq; negate = true; break;

				case clang::BO_LT: type = cfg::nt_less; break;
				case clang::BO_LE: type = cfg::nt_leq; break;
				case clang::BO_GE: type = cfg::nt_less; negate = true; break;
				case clang::BO_GT: type = cfg::nt_leq; negate = true; break;
				default:
					BOOST_ASSERT(0 && "unknown binary operator encountered");
				}

				cfg::vertex_descriptor v = this->add_node(head, enode(type, expr)(lhs)(rhs));
				if (negate)
					v = this->add_node(head, enode(cfg::nt_not, expr)(eot_node, v));
				return eop(eot_node, v);
			}
		}
		else if (clang::UnaryOperator const * e = llvm::dyn_cast<clang::UnaryOperator>(expr))
		{
			if (e->getOpcode() == clang::UO_AddrOf)
			{
				return this->make_address(this->build_expr(head, e->getSubExpr()));
			}
			else if (e->getOpcode() == clang::UO_Deref)
			{
				return this->make_deref(head, this->build_expr(head, e->getSubExpr()));
			}
			else if (e->getOpcode() == clang::UO_PreInc || e->getOpcode() == clang::UO_PreDec)
			{
				eop expr = this->build_expr(head, e->getSubExpr());
				cfg::vertex_descriptor node = this->add_node(head, enode(e)
					(e->getOpcode() == clang::UO_PreInc? cfg::nt_add: cfg::nt_sub)
					(expr)
					(eot_const, sir_int_t(1)));
				this->add_node(head, enode(cfg::nt_assign, e)
					(this->make_address(expr))
					(eot_node, node));
				return expr;
			}
			else if (e->getOpcode() == clang::UO_PostInc || e->getOpcode() == clang::UO_PostDec)
			{
				eop expr = this->build_expr(head, e->getSubExpr());
				cfg::vertex_descriptor node = this->add_node(head, enode(e)
					(e->getOpcode() == clang::UO_PostInc? cfg::nt_add: cfg::nt_sub)
					(expr)
					(eot_const, sir_int_t(1)));
				this->add_node(head, enode(cfg::nt_assign, e)
					(this->make_address(expr))
					(eot_node, node));
				return eop(eot_node, node);
			}
			else if (e->getOpcode() == clang::UO_Plus)
			{
				// Ignore the unary plus operator.
				return this->build_expr(head, e->getSubExpr());
			}
			else if (e->getOpcode() == clang::UO_Not)
			{
				// Model negation as a xor between the value and the maximum value of the value's type.
				uint64_t xor_val = (1 << m_fn->getASTContext().getTypeSize(e->getType())) - 1;
				return eop(eot_node, this->add_node(head, enode(cfg::nt_xor, expr)
					(this->build_expr(head, e->getSubExpr()))
					(eot_const, sir_int_t(xor_val))));
			}
			else
			{
				cfg::node_type type;
				switch (e->getOpcode())
				{
				case clang::UO_Minus: type = cfg::nt_neg; break;
				case clang::UO_LNot: type = cfg::nt_not; break;
				default:
					BOOST_ASSERT(0 && "unknown unary operator encountered");
				}

				return eop(eot_node, this->add_node(head, enode(type, e)
					(this->build_expr(head, e->getSubExpr()))));
			}
		}
		else if (clang::CXXBoolLiteralExpr const * e = llvm::dyn_cast<clang::CXXBoolLiteralExpr>(expr))
		{
			return eop(eot_const, e->getValue()? sir_int_t(1): sir_int_t(0));
		}
		else if (clang::IntegerLiteral const * e = llvm::dyn_cast<clang::IntegerLiteral>(expr))
		{
			return eop(eot_const, (sir_int_t)e->getValue().getLimitedValue());
		}
		else if (clang::FloatingLiteral const * e = llvm::dyn_cast<clang::FloatingLiteral>(expr))
		{
			return eop(eot_const, e->getValueAsApproximateDouble());
		}
		else if (clang::CharacterLiteral const * e = llvm::dyn_cast<clang::CharacterLiteral>(expr))
		{
			return eop(eot_const, sir_int_t(e->getValue()));
		}
		else if (clang::StringLiteral const * e = llvm::dyn_cast<clang::StringLiteral>(expr))
		{
			std::vector<sir_int_t> values = string_literal_to_value_array(e);
			std::string lit_symbol = m_program.get_string_literal_symbol(values, m_static_prefix);
			return eop(eot_var, lit_symbol);
		}
		else if (clang::DeclRefExpr const * e = llvm::dyn_cast<clang::DeclRefExpr>(expr))
		{
			clang::Decl const * decl = e->getDecl();
			if (clang::ValueDecl const * nd = llvm::dyn_cast<clang::ValueDecl>(decl))
			{
				if (clang::EnumConstantDecl const * ecd = llvm::dyn_cast<clang::EnumConstantDecl>(nd))
					return eop(eot_const, sir_int_t(ecd->getInitVal().getLimitedValue()));

				if (clang::FunctionDecl const * fd = llvm::dyn_cast<clang::FunctionDecl>(nd))
				{
					this->register_decl_ref(fd);
					return eop(eot_func, this->get_name(fd));
				}
				
				if (nd->getType()->isReferenceType())
					return eop(eot_vartgt, this->get_name(nd));

				return eop(eot_var, this->get_name(nd));
			}
			else
			{
				BOOST_ASSERT(0 && "encountered a declref to a non-value decl");
				return eop();
			}
		}
		else if (clang::CXXThisExpr const * e = llvm::dyn_cast<clang::CXXThisExpr>(expr))
		{
			return eop(eot_var, "p:this");
		}
		else if (clang::CallExpr const * e = llvm::dyn_cast<clang::CallExpr>(expr))
		{
			// Deal with pseudo-destructor calls.
			if (clang::CXXPseudoDestructorExpr const * de = llvm::dyn_cast<clang::CXXPseudoDestructorExpr>(e->getCallee()))
			{
				return this->build_expr(head, de->getBase());
			}

			// There are several possibilities.
			//  1. The call is a call to an overloaded operator.
			//  2. The expression type is clang::CXXMemberCallExpr. Then the callee is either
			//    a. clang::MemberExpr and the type of the function can be determined from the
			//       member declaration (remember there is an implicit `this` parameter), or
			//    b. clang::BinaryOperator, with either PtrMemD or PtrMemI; the type of
			//       the parameters can be extracted from the rhs operand.
			//  3. This is a normal invocation, in which case the type of the callee is a pointer to function,
			//     the types of parameters can be extracted from there.
			//
			// Additionally, there can be an implicit parameter representing the return value
			// (if the value is of a class type).

			eop callee_op;
			std::vector<eop> params;
			std::vector<clang::Type const *> param_types;
			clang::FunctionProtoType const * fntype;
			std::size_t arg_index = 0;
			if (llvm::isa<clang::CXXOperatorCallExpr>(e))
			{
				BOOST_ASSERT(e->getDirectCallee() != 0);
				if (clang::CXXMethodDecl const * md = llvm::dyn_cast<clang::CXXMethodDecl>(e->getDirectCallee()))
				{
					// C++03: 13.5/6: overloaded operators can't be static member functions
					eop this_op = this->build_expr(head, e->getArg(arg_index++));
					this_op = this->make_address(this_op);
					params.push_back(this_op);

					param_types.push_back(
						md->getThisType(md->getASTContext()).getTypePtr());

					this->register_decl_ref(md);
					callee_op = eop(eot_func, this->get_name(md));
					fntype = llvm::dyn_cast<clang::FunctionProtoType>(md->getType().getTypePtr());
				}
				else
				{
					callee_op = this->build_expr(head, e->getCallee());
					fntype = llvm::dyn_cast<clang::FunctionProtoType>(e->getCallee()->getType()->getPointeeType());
				}
			}
			else if (llvm::isa<clang::CXXMemberCallExpr>(e))
			{
				if (clang::MemberExpr const * mcallee = llvm::dyn_cast<clang::MemberExpr>(e->getCallee()))
				{
					clang::CXXMethodDecl const * mdecl = llvm::dyn_cast<clang::CXXMethodDecl>(mcallee->getMemberDecl());

					eop this_op = this->build_expr(head, mcallee->getBase());
					if (!mdecl->isStatic())
					{
						if (!mcallee->isArrow())
							this_op = this->make_address(this_op);
						params.push_back(this_op);

						param_types.push_back(
							mdecl->getThisType(mdecl->getASTContext()).getTypePtr());
					}
					else
					{
						this->make_node(head, this_op);
					}

					this->register_decl_ref(mdecl);

					std::string fnname = this->get_name(mdecl);
					if (mdecl->isVirtual() && !mcallee->hasQualifier())
						fnname = "v:" + fnname;

					callee_op = eop(eot_func, fnname);
					fntype = llvm::dyn_cast<clang::FunctionProtoType>(mdecl->getType().getTypePtr());
				}
				else if (clang::BinaryOperator const * callee = llvm::dyn_cast<clang::BinaryOperator>(e->getCallee()))
				{
					BOOST_ASSERT(callee->getOpcode() == clang::BO_PtrMemD || callee->getOpcode() == clang::BO_PtrMemI);

					callee_op = this->build_expr(head, callee->getRHS());
					clang::MemberPointerType const * calleePtrType = llvm::dyn_cast<clang::MemberPointerType>(callee->getRHS()->getType().getTypePtr());
					BOOST_ASSERT(calleePtrType);

					eop this_op = this->build_expr(head, callee->getLHS());
					if (callee->getOpcode() == clang::BO_PtrMemD)
						this_op = this->make_address(this_op);
					params.push_back(this_op);

					// FIXME: this should be a pointer to
					param_types.push_back(calleePtrType->getClass()->getCanonicalTypeInternal().getTypePtr());

					fntype = llvm::dyn_cast<clang::FunctionProtoType>(calleePtrType->getPointeeType().getTypePtr());
				}
				else
					BOOST_ASSERT(0);
			}
			else
			{
				callee_op = this->build_expr(head, e->getCallee());
				fntype = llvm::dyn_cast<clang::FunctionProtoType>(e->getCallee()->getType()->getUnqualifiedDesugaredType()->getPointeeType()->getUnqualifiedDesugaredType());
			}

			BOOST_ASSERT(fntype);

			eop result_op;
			clang::Type const * restype = fntype->getResultType().getTypePtr();
			if (restype->isStructureOrClassType())
			{
				eop temp = this->make_temporary(restype);
				params.insert(params.begin(), temp);
				param_types.insert(param_types.begin(), restype);
				result_op = temp;
			}

			BOOST_ASSERT(fntype->isVariadic() || fntype->getNumArgs() + arg_index == e->getNumArgs());

			for (std::size_t i = 0; i < fntype->getNumArgs(); ++i)
			{
				params.push_back(this->build_expr(head, e->getArg(i + arg_index)));
				param_types.push_back(fntype->getArgType(i).getTypePtr());
			}

			for (std::size_t i = fntype->getNumArgs() + arg_index; i < e->getNumArgs(); ++i)
			{
				params.push_back(this->build_expr(head, e->getArg(i)));
				param_types.push_back(e->getArg(i)->getType().getTypePtr());
			}

			// TODO: handle classes with conversion to pointer to fn.
			enode node(cfg::nt_call, e);
			node(callee_op);
			for (std::size_t i = 0; i < params.size(); ++i)
				node(this->make_param(head, params[i], param_types[i]));

			cfg::vertex_descriptor call_node = this->add_node(head, node);

			if (!fntype->hasEmptyExceptionSpec())
				this->connect_to_exc(call_node);

			if (result_op.type != eot_none)
				return result_op;

			if (restype->isReferenceType())
				return eop(eot_nodetgt, call_node);
			else
				return eop(eot_node, call_node);
		}
		else if (clang::ConditionalOperator const * e = llvm::dyn_cast<clang::ConditionalOperator>(expr))
		{
			eop cond_op = this->build_expr(head, e->getCond());
			cfg::vertex_descriptor branch_node = this->make_node(head, cond_op);
			cfg::vertex_descriptor false_head = this->duplicate_vertex(head);
			g[*in_edges(false_head, g).first].cond = sir_int_t(0);

			eop true_res = this->build_expr(head, e->getTrueExpr());
			eop false_res = this->build_expr(false_head, e->getFalseExpr());
			if (true_res.type != eot_none && false_res.type != eot_none)
			{
				return this->make_phi(head, false_head, true_res, false_res, e->isLValue(), e);
			}
			else
			{
				this->join_nodes(false_head, head);
				if (true_res.type == eot_none && false_res.type == eot_none)
					return eop();

				if (true_res.type != eot_none)
					return true_res;
				else
					return false_res;
			}
		}
		else if (clang::SizeOfAlignOfExpr const * e = llvm::dyn_cast<clang::SizeOfAlignOfExpr>(expr))
		{
			// TODO: is there a better way?
			BOOST_ASSERT(e->isSizeOf() && e->isIntegerConstantExpr(m_fn->getASTContext()));
			return eop(eot_const, sir_int_t(e->EvaluateAsInt(m_fn->getASTContext()).getLimitedValue()));
		}
		else if (clang::MemberExpr const * e = llvm::dyn_cast<clang::MemberExpr>(expr))
		{
			// TODO: lvalue/rvalue
			eop base = this->build_expr(head, e->getBase());
			if (!e->isArrow())
				base = this->make_address(base);
			return eop(eot_nodetgt, this->add_node(head, enode(cfg::nt_call, e)
				(eot_member, this->get_name(e->getMemberDecl()))
				(base)));
		}
		else if (clang::ArraySubscriptExpr const * e = llvm::dyn_cast<clang::ArraySubscriptExpr>(expr))
		{
			return this->get_array_element(head, this->build_expr(head, e->getLHS()), this->build_expr(head, e->getRHS()), e);
		}
		else if (clang::ParenExpr const * e = llvm::dyn_cast<clang::ParenExpr>(expr))
		{
			return this->build_expr(head, e->getSubExpr());
		}
		else if (clang::CXXDefaultArgExpr const * e = llvm::dyn_cast<clang::CXXDefaultArgExpr>(expr))
		{
			return this->build_expr(head, e->getExpr());
		}
		else if (clang::CXXConstructExpr const * e = llvm::dyn_cast<clang::CXXConstructExpr>(expr))
		{
			eop temp = this->make_temporary(e->getType().getTypePtr());
			this->build_construct_expr(head, this->make_address(temp), e);
			return temp;
		}
		else if (clang::ExprWithCleanups const * e = llvm::dyn_cast<clang::ExprWithCleanups>(expr))
		{
			return this->build_expr(head, e->getSubExpr());
		}
		else if (clang::CXXBindTemporaryExpr const * e = llvm::dyn_cast<clang::CXXBindTemporaryExpr>(expr))
		{
			// TODO: deal with extended lifetime of temporaries bound to a reference.
			eop res = this->build_expr(head, e->getSubExpr());
			context_node reg = this->register_destructible_var(e->getTemporary()->getDestructor(), this->make_address(res));
			m_fullexpr_lifetimes.back().push_back(reg);
			return res;
		}
		else if (clang::CastExpr const * e = llvm::dyn_cast<clang::CastExpr>(expr))
		{
			// TODO: deal with the casts correctly (notably with dynamic_cast)
			switch (e->getCastKind())
			{
			case clang::CK_ArrayToPointerDecay:
				return this->decay_array_to_pointer(head, this->build_expr(head, e->getSubExpr()));
			default:
				return this->build_expr(head, e->getSubExpr());
			}
		}
		else if (clang::CXXPseudoDestructorExpr const * e = llvm::dyn_cast<clang::CXXPseudoDestructorExpr>(expr))
		{
			return this->build_expr(head, e->getBase());
		}
		else if (clang::CXXNewExpr const * e = llvm::dyn_cast<clang::CXXNewExpr>(expr))
		{
			// TODO: Model the two operators directly.
			if (e->isArray())
			{
				enode node(cfg::nt_cpp_new_array, e);
				node(eot_func, this->get_name(e->getOperatorNew()));
				node(eot_const, sir_int_t(m_fn->getASTContext().getTypeSizeInChars(e->getAllocatedType()).getQuantity()));
				if (e->getConstructor())
					node(eot_func, this->get_name(e->getConstructor()));
				else
					node(eot_const, sir_int_t(0));
				node(eot_func, this->get_name(e->getOperatorDelete()));
				node(eot_const, e->hasInitializer()? sir_int_t(1): sir_int_t(0));
				this->append_args(
					head,
					node,
					e->getOperatorNew()->param_begin() + 1, e->getOperatorNew()->param_end(),
					e->placement_arg_begin(), e->placement_arg_end());

				return eop(eot_node, this->add_node(head, node));
			}
			else
			{
				enode opnew_node(cfg::nt_cpp_new, e);
				opnew_node(eot_func, this->get_name(e->getOperatorNew()));
				opnew_node(eot_const, sir_int_t(m_fn->getASTContext().getTypeSizeInChars(e->getAllocatedType()).getQuantity()));
				this->append_args(
					head,
					opnew_node,
					e->getOperatorNew()->param_begin() + 1, e->getOperatorNew()->param_end(),
					e->placement_arg_begin(), e->placement_arg_end());

				eop ptr_op = eop(eot_node, this->add_node(head, opnew_node));
				if (e->getConstructor() != 0)
				{
					enode construct_node(cfg::nt_call, e);
					construct_node(eot_func, this->get_name(e->getConstructor()));
					construct_node(ptr_op);
					this->append_args(head, construct_node, e->getConstructor()->param_begin(), e->getConstructor()->param_end(),
						e->constructor_arg_begin(), e->constructor_arg_end());
					this->add_node(head, construct_node);
				}

				return ptr_op;
			}
		}
		else if (clang::CXXDeleteExpr const * e = llvm::dyn_cast<clang::CXXDeleteExpr>(expr))
		{
			// TODO: Model directly by calling the destructor followed by calling the operator delete function.
			return eop(eot_node, this->add_node(head, enode(e)
				(e->isArrayForm()? cfg::nt_cpp_delete_array: cfg::nt_cpp_delete)
				(this->build_expr(head, e->getArgument()))
				(eot_func, this->get_name(e->getOperatorDelete()))));
		}
		else if (clang::CXXThrowExpr const * e = llvm::dyn_cast<clang::CXXThrowExpr>(expr))
		{
			if (e->getSubExpr())
			{
				eop exc_mem = eop(eot_node, this->add_node(head, enode(cfg::nt_cpp_exc_alloc)
					(eot_varptr, m_name_mangler.make_rtti_name(e->getType().getUnqualifiedType(), m_static_prefix))));
				
				exc_object_regrec reg = { exc_mem };
				context_node n = m_context_registry.add(reg);

				this->init_object(head, exc_mem, e->getSubExpr()->getType(), e->getSubExpr(), false);

				// Throw the exception object. The throwing will fail if there is an another uncaught exception.
				this->add_node(head, enode(cfg::nt_cpp_exc_throw)
					(exc_mem));

				m_context_registry.remove(n);
				//this->cleanup_lifetime_node(head, n);
			}
			else
			{
				// Retrieve the current exception object.
				cfg::vertex_descriptor exc_object = this->add_node(head, enode(cfg::nt_cpp_exc_current));

				// Rethrow it. The object must currently be unthrown.
				// Note that making the exception object thrown will prevent it from being freed.
				this->add_node(head, enode(cfg::nt_cpp_exc_throw)
					(eot_node, exc_object));
			}

			m_exc_registry[m_context_registry.current_context()].push_back(head);
			head = add_vertex(g);
			return eop();
		}
		else
		{
			BOOST_ASSERT(0 && "unknown AST node encountered");
			return eop();
		}
	}

	eop get_array_element(cfg::vertex_descriptor & head, eop const & decayedptr, eop const & index, clang::Stmt const * data)
	{
		if (index.type == eot_const && get_const<sir_int_t>(index.id) == 0)
			return this->make_deref(head, decayedptr);

		return eop(eot_nodetgt, this->add_node(head, enode(cfg::nt_add, data)
			(decayedptr)
			(index)));
	}

	eop build_full_expr(cfg::vertex_descriptor & head, clang::Expr const * expr)
	{
		this->begin_lifetime_context(m_fullexpr_lifetimes);
		eop res = this->build_expr(head, expr);
		this->end_lifetime_context(head, m_fullexpr_lifetimes);
		return res;
	}

	eop decay_array_to_pointer(cfg::vertex_descriptor & head, eop const & arr)
	{
		// WISH: When we do cfg typing, fix this to deal with string literals correctly.
		if (arr.type == eot_const)
			return arr;

		// The array must be an lvalue, otherwise we cannot get a pointer to the first element.
		BOOST_ASSERT(this->is_lvalue(arr));
		return eop(eot_node, this->add_node(head, enode(cfg::nt_decay)
			(this->make_address(arr))));
	}

	void zero_initialize(cfg::vertex_descriptor & head, eop const & varptr, clang::QualType vartype, bool blockLifetime, clang::Stmt const * data)
	{
		BOOST_ASSERT(vartype->isScalarType());
		this->add_node(head, enode(cfg::nt_assign, data)
			(varptr)
			(eot_const, sir_int_t(0)));
	}

	void value_initialize(cfg::vertex_descriptor & head, eop const & varptr, clang::QualType vartype, bool blockLifetime, clang::Stmt const * data = 0)
	{
		BOOST_ASSERT(!vartype->isStructureOrClassType() && "calls to constructors should be injected to AST by sema");

		if (clang::ArrayType const * at = llvm::dyn_cast<clang::ArrayType>(vartype))
		{
			BOOST_ASSERT(llvm::isa<clang::ConstantArrayType>(at));
			llvm::APInt const & size = llvm::dyn_cast<clang::ConstantArrayType>(at)->getSize();

			eop decayedptr = this->decay_array_to_pointer(head, this->make_deref(head, varptr));
			for (llvm::APInt i(size.getBitWidth(), 0); i != size; ++i)
				this->value_initialize(head, this->get_array_element(head, decayedptr, eop(eot_const, sir_int_t(i.getLimitedValue())), data), at->getElementType(), blockLifetime, data);
		}
		else
		{
			this->zero_initialize(head, varptr, vartype, blockLifetime, data);
		}
	}

	void init_object(cfg::vertex_descriptor & head, eop const & varptr, clang::QualType vartype, clang::Expr const * e, bool blockLifetime)
	{
		BOOST_ASSERT(e != 0);
		BOOST_ASSERT(!m_fullexpr_lifetimes.empty());

		if (clang::ExprWithCleanups const * te = llvm::dyn_cast<clang::ExprWithCleanups>(e))
		{
			this->init_object(head, varptr, vartype, te->getSubExpr(), blockLifetime);
			return;
		}

		vartype = vartype->getCanonicalTypeUnqualified();

		// TODO: check fullexpr lifetimes
		if (vartype->isStructureOrClassType())
		{
			if (clang::CXXConstructExpr const * ce = llvm::dyn_cast<clang::CXXConstructExpr>(e))
			{
				this->build_construct_expr(head, varptr, llvm::dyn_cast<clang::CXXConstructExpr>(e));
				if (blockLifetime)
				{
					clang::CXXRecordDecl const * recordDecl = vartype->getAsCXXRecordDecl();
					if (recordDecl && recordDecl->hasDeclaredDestructor())
					{
						m_block_lifetimes.back().push_back(this->register_destructible_var(recordDecl->getDestructor(), varptr));
					}
				}
			}
			else if (clang::InitListExpr const * ie = llvm::dyn_cast<clang::InitListExpr>(e))
			{
				clang::CXXRecordDecl const * recordDecl = vartype->getAsCXXRecordDecl();

				BOOST_ASSERT(recordDecl->getNumBases() == 0);
				unsigned init_idx = 0;
				for (clang::CXXRecordDecl::field_iterator it = recordDecl->field_begin(); it != recordDecl->field_end(); ++it, ++init_idx)
				{
					clang::FieldDecl const * field = *it;

					eop fieldptrop = eop(eot_node, this->add_node(head, enode(cfg::nt_call)
						(eot_member, this->get_name(field))
						(varptr)));

					// TODO: exceptions?
					if (init_idx < ie->getNumInits())
						this->init_object(head, fieldptrop, field->getType(), ie->getInit(init_idx), false);
					else
						this->value_initialize(head, fieldptrop, field->getType(), blockLifetime);
				}
			}
			else
			{
				BOOST_ASSERT(0 && "unknown type of initialization encountered");
			}
		}
		else if (vartype->isReferenceType())
		{
			// TODO: extend the lifetime of temporaries
			this->add_node(head, enode(cfg::nt_assign, e)
				(varptr)
				(this->make_address(this->build_expr(head, e))));
		}
		else if (vartype->isArrayType())
		{
			BOOST_ASSERT(llvm::isa<clang::ConstantArrayType>(vartype));
			clang::ConstantArrayType const * at = llvm::cast<clang::ConstantArrayType>(vartype);
			eop decayedptr = this->decay_array_to_pointer(head, this->make_deref(head, varptr));

			llvm::APInt i(at->getSize().getBitWidth(), 0);
			if (clang::InitListExpr const * ile = llvm::dyn_cast<clang::InitListExpr>(e))
			{
				// TODO: exception safety
				unsigned init_idx = 0;
				for (; i != at->getSize() && init_idx != ile->getNumInits(); ++i, ++init_idx)
				{
					eop elem = this->get_array_element(head, decayedptr, eop(eot_const, sir_int_t(i.getLimitedValue())), e);
					this->init_object(head, this->make_address(elem), at->getElementType(), ile->getInit(init_idx), blockLifetime);
				}
			}
			else if (clang::StringLiteral const * sl = llvm::dyn_cast<clang::StringLiteral>(e))
			{
				std::vector<sir_int_t> values = string_literal_to_value_array(sl);

				std::size_t init_idx = 0;
				for (; i != at->getSize() && init_idx < values.size(); ++i, ++init_idx)
				{
					eop elem = this->get_array_element(head, decayedptr, eop(eot_const, sir_int_t(i.getLimitedValue())), e);
					this->add_node(head, enode(cfg::nt_assign, e)
						(this->make_address(elem))
						(eot_const, sir_int_t(values[init_idx])));
				}
			}
			else if (clang::ArraySubscriptExpr const * ie = llvm::dyn_cast<clang::ArraySubscriptExpr>(e))
			{
				// TODO
				i = at->getSize();
			}
			else if (clang::CXXConstructExpr const * ie = llvm::dyn_cast<clang::CXXConstructExpr>(e))
			{
				// TODO
				i = at->getSize();
			}
			else
			{
				BOOST_ASSERT(0 && "unrecognized array initializer");
			}

			if (i != at->getSize())
			{
				eop loop_counter = this->make_temporary(m_fn->getASTContext().getSizeType()->getTypePtr());
				this->add_node(head, enode(cfg::nt_assign, e)
					(this->make_address(loop_counter))
					(eot_const, sir_int_t(i.getLimitedValue())));
				cfg::vertex_descriptor cond_node = this->add_node(head, enode(cfg::nt_less, e)
					(loop_counter)
					(eot_const, sir_int_t(at->getSize().getLimitedValue())));

				cfg::vertex_descriptor false_head = this->duplicate_vertex(head);
				this->set_cond(false_head, 0, sir_int_t(0));

				eop elem = this->get_array_element(head, decayedptr, loop_counter, e);
				this->value_initialize(head, this->make_address(elem), at->getElementType(), blockLifetime, e);

				cfg::vertex_descriptor incremented = this->add_node(head, enode(cfg::nt_add, e)
					(loop_counter)
					(eot_const, sir_int_t(1)));
				this->add_node(head, enode(cfg::nt_assign, e)
					(this->make_address(loop_counter))
					(eot_node, incremented));

				this->join_nodes(head, cond_node);
				head = false_head;
			}
		}
		else
		{
			if (clang::InitListExpr const * ie = llvm::dyn_cast<clang::InitListExpr>(e))
			{
				BOOST_ASSERT(ie->getNumInits() == 1);
				this->init_object(head, varptr, vartype, ie->getInit(0),blockLifetime);
			}
			else if (clang::ImplicitValueInitExpr const * ie = llvm::dyn_cast<clang::ImplicitValueInitExpr>(e))
			{
				this->value_initialize(head, varptr, vartype, blockLifetime);
			}
			else
			{
				this->add_node(head, enode(cfg::nt_assign, e)
					(varptr)
					(this->build_expr(head, e)));
			}
		}
	}

	void build_stmt(cfg::vertex_descriptor & head, clang::Stmt const * stmt)
	{
		BOOST_ASSERT(stmt != 0);

		m_visitor.statement_visited(stmt);

		if (clang::CompoundStmt const * s = llvm::dyn_cast<clang::CompoundStmt>(stmt))
		{
			this->begin_lifetime_context(m_block_lifetimes);
			for (clang::CompoundStmt::const_body_iterator it = s->body_begin(); it != s->body_end(); ++it)
				this->build_stmt(head, *it);
			this->end_lifetime_context(head, m_block_lifetimes);
		}
		else if (clang::Expr const * s = llvm::dyn_cast<clang::Expr>(stmt))
		{
			this->build_full_expr(head, s);
		}
		else if (clang::ReturnStmt const * s = llvm::dyn_cast<clang::ReturnStmt>(stmt))
		{
			eop val;
			if (s->getRetValue() != 0)
			{
				if (m_fn->getResultType()->isStructureOrClassType())
				{
					this->begin_lifetime_context(m_fullexpr_lifetimes);
					this->init_object(head, eop(eot_var, "p:return"), m_fn->getResultType(), s->getRetValue(), false);
					this->end_lifetime_context(head, m_fullexpr_lifetimes);
				}
				else
				{
					val = this->build_full_expr(head, s->getRetValue());
					if (m_fn->getResultType()->isReferenceType())
						val = this->make_address(val);
				}
			}

			m_return_registry[m_context_registry.current_context()].push_back(jump_sentinel(head, val));
			head = add_vertex(g);
		}
		else if (clang::BreakStmt const * s = llvm::dyn_cast<clang::BreakStmt>(stmt))
		{
			BOOST_ASSERT(!m_break_registries.empty());
			m_break_registries.back()[m_context_registry.current_context()].push_back(head);
			head = add_vertex(g);
		}
		else if (clang::ContinueStmt const * s = llvm::dyn_cast<clang::ContinueStmt>(stmt))
		{
			BOOST_ASSERT(!m_continue_registries.empty());
			m_continue_registries.back()[m_context_registry.current_context()].push_back(head);
			head = add_vertex(g);
		}
		else if (clang::IfStmt const * s = llvm::dyn_cast<clang::IfStmt>(stmt))
		{
			this->make_node(head, this->build_full_expr(head, s->getCond()));
			cfg::vertex_descriptor else_head = this->duplicate_vertex(head);

			this->set_cond(else_head, 0, sir_int_t(0));

			this->build_stmt(head, s->getThen());
			if (s->getElse() != 0)
				this->build_stmt(else_head, s->getElse());
			this->join_nodes(else_head, head);
		}
		else if (clang::WhileStmt const * s = llvm::dyn_cast<clang::WhileStmt>(stmt))
		{
			cfg::vertex_descriptor cond_start = head;
			this->make_cond_node(head, this->build_full_expr(head, s->getCond()));
			cfg::vertex_descriptor body_head = this->duplicate_vertex(head);
			this->set_cond(head, 0, sir_int_t(0));

			execution_context outer_ec = m_context_registry.current_context();

			m_break_registries.push_back(jump_registry());
			m_continue_registries.push_back(jump_registry());

			this->build_stmt(body_head, s->getBody());
			this->join_nodes(body_head, cond_start);

			this->join_jump_registry(m_break_registries, outer_ec, head);
			this->join_jump_registry(m_continue_registries, outer_ec, cond_start);
		}
		else if (clang::DoStmt const * s = llvm::dyn_cast<clang::DoStmt>(stmt))
		{
			cfg::vertex_descriptor start_node = head;

			execution_context outer_ec = m_context_registry.current_context();

			m_break_registries.push_back(jump_registry());
			m_continue_registries.push_back(jump_registry());

			this->build_stmt(head, s->getBody());

			cfg::vertex_descriptor cond_node = this->make_cond_node(head, this->build_full_expr(head, s->getCond()));
			cfg::vertex_descriptor loop_node = this->duplicate_vertex(head);
			this->set_cond(head, 0, sir_int_t(0));

			this->join_nodes(loop_node, start_node);

			this->join_jump_registry(m_break_registries, outer_ec, head);
			this->join_jump_registry(m_continue_registries, outer_ec, cond_node);
		}
		else if (clang::ForStmt const * s = llvm::dyn_cast<clang::ForStmt>(stmt))
		{
			// FIXME: a virtual block scope should enclose the for loop
			if (s->getInit())
				this->build_stmt(head, s->getInit());

			execution_context outer_ec = m_context_registry.current_context();

			cfg::vertex_descriptor start_node = head;
			cfg::vertex_descriptor cond_node = head;
			cfg::vertex_descriptor exit_node;
			if (s->getCond())
			{
				cond_node = this->make_cond_node(head, this->build_full_expr(head, s->getCond()));
				exit_node = this->duplicate_vertex(head);
				this->set_cond(exit_node, 0, sir_int_t(0));
			}
			else
				exit_node = add_vertex(g);

			m_break_registries.push_back(jump_registry());
			m_continue_registries.push_back(jump_registry());

			cfg::vertex_descriptor body_node = head;
			this->build_stmt(head, s->getBody());
			this->join_jump_registry(m_continue_registries, outer_ec, head);

			if (s->getInc())
				this->build_full_expr(head, s->getInc());
			this->join_nodes(head, start_node);
			head = exit_node;

			this->join_jump_registry(m_break_registries, outer_ec, exit_node);
		}
		else if (clang::DefaultStmt const * s = llvm::dyn_cast<clang::DefaultStmt>(stmt))
		{
			BOOST_ASSERT(!m_case_contexts.empty());
			BOOST_ASSERT(m_case_contexts.back().first == cfg::null_vertex());
			m_case_contexts.back().first = head;
			this->build_stmt(head, s->getSubStmt());
		}
		else if (clang::CaseStmt const * s = llvm::dyn_cast<clang::CaseStmt>(stmt))
		{
			BOOST_ASSERT(!m_case_contexts.empty());
			BOOST_ASSERT(s->getRHS() == 0 && "case lhs..rhs; gcc extension is not supported");

			eop cond = eop(eot_const, sir_int_t(s->getLHS()->EvaluateAsInt(m_fn->getASTContext()).getLimitedValue()));

			m_case_contexts.back().second[get_const<sir_int_t>(cond.id)] = head;
			this->build_stmt(head, s->getSubStmt());
		}
		else if (clang::SwitchStmt const * s = llvm::dyn_cast<clang::SwitchStmt>(stmt))
		{
			cfg::vertex_descriptor cond_node = this->make_node(head, this->build_full_expr(head, s->getCond()));
			cfg::vertex_descriptor cond_cont = head;
			cfg::vertex_descriptor body_start = add_vertex(g);
			head = body_start;

			execution_context outer_ec = m_context_registry.current_context();

			m_case_contexts.push_back(case_context_t());
			m_break_registries.push_back(jump_registry());
			this->build_stmt(head, s->getBody());

			this->join_jump_registry(m_break_registries, outer_ec, head);

			case_context_t const & case_ctx = m_case_contexts.back();
			if (case_ctx.first != cfg::null_vertex())
				this->join_nodes(cond_cont, case_ctx.first);
			else
				this->join_nodes(cond_cont, body_start);

			for (case_context_t::second_type::const_iterator it = case_ctx.second.begin(); it != case_ctx.second.end(); ++it)
			{
				cfg::edge_descriptor e = add_edge(cond_node, it->second, g).first;
				g[e].cond = it->first;
			}

			m_case_contexts.pop_back();
		}
		else if (clang::DeclStmt const * s = llvm::dyn_cast<clang::DeclStmt>(stmt))
		{
			for (clang::DeclStmt::const_decl_iterator ci = s->decl_begin(); ci != s->decl_end(); ++ci)
			{
				clang::Decl const * decl = *ci;
				if (clang::VarDecl const * vd = llvm::dyn_cast<clang::VarDecl>(decl))
				{
					// TODO: deal with static variables correctly
					if (vd->isStaticLocal())
						continue;

					if (vd->hasInit())
					{
						this->begin_lifetime_context(m_fullexpr_lifetimes);
						this->init_object(head, eop(eot_varptr, this->get_name(vd)), vd->getType(), vd->getInit(), true);
						this->end_lifetime_context(head, m_fullexpr_lifetimes);
					}
				}
			}
		}
		else if (clang::NullStmt const * s = llvm::dyn_cast<clang::NullStmt>(stmt))
		{
			// Leave a comment about the null statement.
			this->add_node(head, enode(cfg::nt_none)(eot_const, "null"));
		}
		else if (clang::CXXTryStmt const * s = llvm::dyn_cast<clang::CXXTryStmt>(stmt))
		{
			BOOST_ASSERT(s->getNumHandlers() != 0);

			cfg::vertex_descriptor handler_head = add_vertex(g);
			except_regrec reg = { handler_head };

			eop exc_object = eop(eot_node, this->add_node(handler_head, enode(cfg::nt_cpp_exc_current)));

			std::vector<cfg::vertex_descriptor> handled_heads;
			for (unsigned i = 0; i != s->getNumHandlers(); ++i)
			{
				clang::CXXCatchStmt const * handler = s->getHandler(i);
				if (handler->getExceptionDecl())
				{
					// Attempt to match the exception object to the handler.
					// If the matching succeeds, the exception is marked as caught.
					cfg::vertex_descriptor match_node = this->add_node(handler_head, enode(cfg::nt_cpp_exc_catch)
						(exc_object)
						(eot_varptr, m_name_mangler.make_rtti_name(handler->getCaughtType(), m_static_prefix)));

					cfg::vertex_descriptor false_head = this->duplicate_vertex(handler_head);
					this->set_cond(false_head, 0, sir_int_t(0));

					exc_object_regrec reg = { exc_object };
					context_node n = m_context_registry.add(reg);

					clang::VarDecl const * var = handler->getExceptionDecl();
					if (var->getIdentifier())
					{
						if (var->getType()->isReferenceType())
						{
							this->add_node(handler_head, enode(cfg::nt_assign)
								(eot_varptr, this->get_name(var))
								(eot_node, match_node));
						}
						else if  (var->getType()->isScalarType() || var->getType()->isPointerType())
							this->add_node(handler_head, enode(cfg::nt_assign)
								(eot_varptr, this->get_name(var))
								(eot_nodetgt, match_node));
						else
						{
							// TODO: implement catch by value
							BOOST_ASSERT(0 && "catch by value for complex types is not implemented yet");
						}
					}

					this->build_stmt(handler_head, handler->getHandlerBlock());
					handled_heads.push_back(handler_head);
					handler_head = false_head;

					m_context_registry.remove(n);
				}
				else
				{
					exc_object_regrec reg = { exc_object };
					context_node n = m_context_registry.add(reg);

					this->build_stmt(handler_head, handler->getHandlerBlock());
					handled_heads.push_back(handler_head);
					handler_head = add_vertex(g);

					m_context_registry.remove(n);
				}
			}

			m_exc_registry[m_context_registry.current_context()].push_back(handler_head);

			BOOST_ASSERT(!handled_heads.empty());
			for (std::size_t i = 1; i < handled_heads.size(); ++i)
				this->join_nodes(handled_heads[i], handled_heads[0]);

			// Release the exception object. Note that uncaught object will not be released
			// so that rethrow is possible.
			cfg::vertex_descriptor v = this->add_node(handled_heads[0], enode(cfg::nt_cpp_exc_free)
				(exc_object));

			context_node n = m_context_registry.add(reg);
			this->build_stmt(head, s->getTryBlock());
			m_context_registry.remove(n);

			this->join_nodes(handled_heads[0], head);
		}
		else if (clang::LabelStmt const * s = llvm::dyn_cast<clang::LabelStmt>(stmt))
		{
			m_labels[s] = head;
			this->build_stmt(head, s->getSubStmt());
		}
		else if (clang::GotoStmt const * s = llvm::dyn_cast<clang::GotoStmt>(stmt))
		{
			m_gotos[s] = head;
			head = add_vertex(g);
		}
		else if (clang::AsmStmt const * s = llvm::dyn_cast<clang::AsmStmt>(stmt))
		{
			// Leave a comment about the asm statement but ignore its contents.
			this->add_node(head, enode(cfg::nt_none)(eot_const, "asm")(eot_const, std::string(s->getAsmString()->getString())));
		}
		else
		{
			// IndirectGotoStmt
			// ObjC statements
			BOOST_ASSERT(0 && "unknown AST node encountered");
		}
	}

	void backpatch_gotos()
	{
		for (std::map<clang::GotoStmt const *, cfg::vertex_descriptor>::const_iterator it = m_gotos.begin(); it != m_gotos.end(); ++it)
		{
			BOOST_ASSERT(it->first != 0);
			std::map<clang::LabelStmt const *, cfg::vertex_descriptor>::const_iterator label_it = m_labels.find(it->first->getLabel());
			BOOST_ASSERT(label_it != m_labels.end());
			this->join_nodes(it->second, label_it->second);
		}
	}

	void attach_range_tag_to_exit_nodes()
	{
		std::pair<cfg::vertex_iterator, cfg::vertex_iterator> vertex_range = vertices(g);
		for (; vertex_range.first != vertex_range.second; ++vertex_range.first)
		{
			cfg::node & n = g[*vertex_range.first];
			if (n.type == cfg::nt_exit)
				this->attach_range_tag(n, m_fn->getBodyRBrace());
		}
	}

	struct return_path_generator
		: boost::static_visitor<jump_sentinel>
	{
		return_path_generator(context & ctx, execution_context ec, jump_sentinel const & s)
			: ctx(ctx), ec(ec), s(s)
		{
		}

		jump_sentinel operator()(var_regrec const & reg)
		{
			cfg::vertex_descriptor node = ctx.add_node(s.sentinel, enode(cfg::nt_call)
				(eot_func, ctx.get_name(reg.destr))
				(reg.varptr));
			if (!llvm::cast<clang::FunctionProtoType>(reg.destr->getType()->getUnqualifiedDesugaredType())->hasEmptyExceptionSpec())
				ctx.connect_to_exc(node, ec);
			return s;
		}

		jump_sentinel operator()(exc_object_regrec const & reg)
		{
			ctx.add_node(s.sentinel, enode(cfg::nt_cpp_exc_free)
				(reg.exc_obj_ptr));
			return s;
		}

		template <typename T>
		jump_sentinel operator()(T const & t)
		{
			return s;
		}

		context & ctx;
		execution_context ec;
		jump_sentinel s;
	};

	struct exception_path_generator
		: boost::static_visitor<jump_sentinel>
	{
		exception_path_generator(context & ctx, execution_context ec, jump_sentinel const & s)
			: ctx(ctx), ec(ec), s(s)
		{
		}

		jump_sentinel operator()(var_regrec const & reg)
		{
			cfg::vertex_descriptor node = ctx.add_node(s.sentinel, enode(cfg::nt_call)
				(eot_func, ctx.get_name(reg.destr))
				(reg.varptr));
			return s;
		}

		jump_sentinel operator()(except_regrec const & reg)
		{
			ctx.join_nodes(s.sentinel, reg.entry_node);
			return jump_sentinel(ctx.g.null_vertex(), eop());
		}

		jump_sentinel operator()(exc_object_regrec const & reg)
		{
			ctx.add_node(s.sentinel, enode(cfg::nt_cpp_exc_free)
				(reg.exc_obj_ptr));
			return s;
		}

		template <typename T>
		jump_sentinel operator()(T const & t)
		{
			return s;
		}

		context & ctx;
		execution_context ec;
		jump_sentinel s;
	};

	template <typename Generator>
	jump_sentinel generate_oob_paths(jump_registry & registry, execution_context last_ctx)
	{
		if (registry.empty())
			return jump_sentinel(g.null_vertex(), eop());

		for (jump_registry::reverse_iterator it = registry.rbegin(); it != registry.rend() && it->first != last_ctx; ++it)
		{
			BOOST_ASSERT(it->first >= last_ctx);

			jump_sentinel s = this->join_jump_sentinels(it->second);

			Generator g(*this, m_context_registry.parent(it->first), s);
			s = m_context_registry.value(it->first).apply_visitor(g);

			if (s.sentinel != this->g.null_vertex())
				registry[m_context_registry.parent(it->first)].push_back(s);
		}

		BOOST_ASSERT(registry.begin()->first == last_ctx);
		BOOST_ASSERT(!registry.begin()->second.empty());

		return this->join_jump_sentinels(registry.begin()->second);
	}

	jump_sentinel generate_return_paths(jump_registry & registry, execution_context last_ctx)
	{
		return this->generate_oob_paths<return_path_generator>(registry, last_ctx);
	}

	jump_sentinel generate_exception_paths(jump_registry & registry, execution_context last_ctx)
	{
		return this->generate_oob_paths<exception_path_generator>(registry, last_ctx);
	}

	jump_sentinel join_jump_sentinels(std::vector<jump_sentinel> & sentinels)
	{
		BOOST_ASSERT(!sentinels.empty());

		if (sentinels.size() == 1)
			return sentinels[0];

		if (sentinels[0].value.type == eot_none)
		{
			for (std::size_t i = 1; i < sentinels.size(); ++i)
				this->join_nodes(sentinels[i].sentinel, sentinels[0].sentinel);

			return jump_sentinel(sentinels[0].sentinel, eop());
		}

		enode phi_node(cfg::nt_phi);
		phi_node(eot_node, this->make_node(sentinels[0].sentinel, sentinels[0].value));
		for (std::size_t i = 1; i < sentinels.size(); ++i)
		{
			phi_node(eot_node, this->make_node(sentinels[i].sentinel, sentinels[i].value));
			this->join_nodes(sentinels[i].sentinel, sentinels[0].sentinel);
		}

		eop res(eot_node, this->add_node(sentinels[0].sentinel, phi_node));
		return jump_sentinel(sentinels[0].sentinel, res);
	}
	
	void finish()
	{
		this->backpatch_gotos();

		// Generate return paths. The current head forms a default return path.
		{
			jump_sentinel exit_sentinel = this->generate_return_paths(m_return_registry, 0);

			cfg::node exit_node(cfg::nt_exit);
			exit_node.ops.push_back(cfg::operand(cfg::ot_const, sir_int_t(0)));
			if (exit_sentinel.value.type != eot_none)
			{
				BOOST_ASSERT(exit_sentinel.sentinel != g.null_vertex());
				exit_node.ops.push_back(this->make_rvalue(exit_sentinel.sentinel, exit_sentinel.value));
				clear_vertex(m_head, g);
				remove_vertex(m_head, g);
			}
			else
			{
				if (exit_sentinel.sentinel != g.null_vertex())
					this->join_nodes(m_head, exit_sentinel.sentinel);
				else
					exit_sentinel.sentinel = m_head;
			}
			g[exit_sentinel.sentinel] = exit_node;
		}

		jump_sentinel exc_sentinel = this->generate_exception_paths(m_exc_registry, 0);
		if (exc_sentinel.sentinel != g.null_vertex())
		{
			cfg::node exc_node(cfg::nt_exit);
			exc_node.ops.push_back(cfg::operand(cfg::ot_const, sir_int_t(1)));
			g[exc_sentinel.sentinel] = exc_node;
		}

		this->simplify();
		this->attach_range_tag_to_exit_nodes();
	}

	void simplify()
	{
		std::vector<cfg::vertex_descriptor> unique_vertices;
		std::pair<cfg::vertex_iterator, cfg::vertex_iterator> vertex_range = vertices(g);
		std::copy(vertex_range.first, vertex_range.second, std::back_inserter(unique_vertices));

		std::map<cfg::vertex_descriptor, cfg::vertex_descriptor> merges;
		
		for (std::size_t i = 0; i != unique_vertices.size(); ++i)
		{
			merges[unique_vertices[i]] = unique_vertices[i];
			for (std::size_t j = i + 1; j != unique_vertices.size(); ++j)
			{
				cfg::vertex_descriptor u = unique_vertices[i];
				cfg::vertex_descriptor v = unique_vertices[j];

				if (g[u].type != g[v].type || g[u].ops.size() != g[v].ops.size() || g[u].tags != g[v].tags || out_degree(u, g) != out_degree(v, g))
					continue;

				if (!std::equal(g[u].ops.begin(), g[u].ops.end(), g[v].ops.begin()))
					continue;

				std::pair<cfg::out_edge_iterator, cfg::out_edge_iterator> out_edges_range = out_edges(u, g);
				std::vector<std::pair<cfg::edge, cfg::vertex_descriptor> > u_out_edges;
				for (; out_edges_range.first != out_edges_range.second; ++out_edges_range.first)
					u_out_edges.push_back(std::make_pair(g[*out_edges_range.first], target(*out_edges_range.first, g)));

				bool ok = true;
				out_edges_range = out_edges(v, g);
				for (; ok && out_edges_range.first != out_edges_range.second; ++out_edges_range.first)
				{
					if (std::find(u_out_edges.begin(), u_out_edges.end(), std::make_pair(g[*out_edges_range.first], target(*out_edges_range.first, g))) == u_out_edges.end())
						ok = false;
				}

				if (!ok)
					continue;

				merges[unique_vertices[j]] = unique_vertices[i];

				using std::swap;
				swap(unique_vertices[j], unique_vertices.back());
				unique_vertices.pop_back();
				--j;
			}
		}

		for (std::size_t i = 0; i != unique_vertices.size(); ++i)
		{
			cfg::node & node = g[unique_vertices[i]];
			for (std::size_t j = 0; j != node.ops.size(); ++j)
			{
				if (node.ops[j].type == cfg::ot_node)
					node.ops[j].id = merges[boost::get<cfg::vertex_descriptor>(node.ops[j].id)];
			}
		}

		for (std::map<cfg::vertex_descriptor, cfg::vertex_descriptor>::const_iterator it = merges.begin(); it != merges.end(); ++it)
		{
			if (it->first == it->second)
				continue;

			std::pair<cfg::in_edge_iterator, cfg::in_edge_iterator> in_edges_range = in_edges(it->first, g);
			for (; in_edges_range.first != in_edges_range.second; ++in_edges_range.first)
			{
				cfg::edge_descriptor e = add_edge(source(*in_edges_range.first, g), it->second, g).first;
				g[e] = g[*in_edges_range.first];
			}

			if (g.entry() == it->first)
				g.entry(it->second);

			clear_vertex(it->first, g);
			remove_vertex(it->first, g);
		}
	}

	void build_constructor(clang::CXXConstructorDecl const * fn)
	{
		// TODO: handle virtual bases properly

		m_block_lifetimes.push_back(lifetime_context_t());
		for (clang::CXXConstructorDecl::init_const_iterator it = fn->init_begin(); it != fn->init_end(); ++it)
		{
			clang::CXXCtorInitializer const * init = *it;
			if (init->isMemberInitializer())
			{
				BOOST_ASSERT(!init->isBaseInitializer());
				cfg::vertex_descriptor memberop = this->add_node(m_head, enode(cfg::nt_call)
					(eot_member, this->get_name(init->getMember()))
					(eot_var, "p:this"));

				this->begin_lifetime_context(m_fullexpr_lifetimes);
				this->init_object(m_head, eop(eot_node, memberop), init->getMember()->getType(), init->getInit(), false);
				this->end_lifetime_context(m_head, m_fullexpr_lifetimes);

				// TODO: figure out how to do exception safety here
			}
			else
			{
				// TODO: Convert p:this
				BOOST_ASSERT(init->isBaseInitializer());

				this->begin_lifetime_context(m_fullexpr_lifetimes);
				this->init_object(m_head, eop(eot_var, "p:this"), clang::QualType(init->getBaseClass(), 0), init->getInit(), false);
				this->end_lifetime_context(m_head, m_fullexpr_lifetimes);

				// TODO: figure out how to do exception safety here
			}
		}
		
		// The lifetime gets extended and is guarded by the destructor of this class.
		// TODO: Kill the object in the case of an exception.
		m_block_lifetimes.pop_back();
	}

	void build_destructor(clang::CXXDestructorDecl const * fn)
	{
		clang::CXXRecordDecl const * rec = fn->getParent();
		// TODO: handle virtual bases

		std::vector<clang::FieldDecl const *> fields;
		for (clang::RecordDecl::field_iterator field_it = rec->field_end(); field_it != rec->field_end(); ++field_it)
			fields.push_back(*field_it);

		for (std::size_t i = fields.size(); i != 0; --i)
		{
			clang::FieldDecl const * fd = fields[i-1];
			if (clang::CXXRecordDecl const * rd = fd->getType()->getAsCXXRecordDecl())
			{
				if (rd->hasDeclaredDestructor() && !rd->getDestructor()->isTrivial())
				{
					this->register_decl_ref(rd->getDestructor());

					cfg::vertex_descriptor member = this->add_node(m_head, enode(cfg::nt_call)
						(eot_member, this->get_name(fd))
						(eot_var, "p:this"));

					this->add_node(m_head, enode(cfg::nt_call)
						(eot_func, this->get_name(rd->getDestructor()))
						(eot_node, member));
				}
			}
		}

		for (clang::CXXRecordDecl::reverse_base_class_const_iterator it = rec->bases_rbegin(); it != rec->bases_rend(); ++it)
		{
			clang::CXXBaseSpecifier const & bs = *it;

			if (bs.isVirtual())
				continue;

			if (clang::CXXRecordDecl const * rd = bs.getType()->getAsCXXRecordDecl())
			{
				if (rd->hasDeclaredDestructor() && !rd->getDestructor()->isTrivial())
				{
					this->register_decl_ref(rd->getDestructor());

					// TODO: proper conversion, exceptions
					this->add_node(m_head, enode(cfg::nt_call)
						(eot_func, this->get_name(rd->getDestructor()))
						(eot_var, "p:this"));
				}
			}
		}
	}

	void build()
	{
		BOOST_ASSERT(!m_fn->isDependentContext());

		this->register_locals(m_fn);
		if (clang::CXXConstructorDecl const * cd = llvm::dyn_cast<clang::CXXConstructorDecl>(m_fn))
			this->build_constructor(cd);
		if (m_fn->hasBody())
			this->build_stmt(m_head, m_fn->getBody());
		if (clang::CXXDestructorDecl const * cd = llvm::dyn_cast<clang::CXXDestructorDecl>(m_fn))
			this->build_destructor(cd);
		this->finish();
	}
};

}

void detail::build_cfg(program & p, cfg & c, name_mangler & nm, clang::FunctionDecl const * fn, clang::SourceManager const & sm,
	filename_store & fnames, build_cfg_visitor_base & visitor, std::string const & static_prefix)
{
	context(p, c, nm, fn, sm, fnames, visitor, static_prefix);
}
