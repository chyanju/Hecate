#include<vector>
using namespace std;
enum NodeType { LEAF, INNER };

class Poly {
public:
	vector<float> arr;
	int size;
	void assign_coeff(int _size) {
		size = _size;
		arr.resize(size, 0);
	}
	void differentiate() {
		for (int i=0; i<size-1; i++) {
			arr[i] = (i+1) * arr[i+1];
		}
		size--;
	}
	void set_array(vector<float> &_arr, int _size) {
		arr = _arr;
		size = _size;
	}
	void square() {
		vector<float> sarr;
		int N = size;
		int M = N * N;
		sarr.resize(M, 0);
		for (int i=0; i<size; i++) {
			for (int j=0; j<size; j++) {
				sarr[i+j] += arr[i] * arr[j];
			}
		}
		set_array(sarr, M);
	}
	void add_const(float c) {
		for (int i=0; i<size; i++) {
			arr[i] += c;
		}
	}
	void mul_const(float c) {
		for (int i=0; i<size; i++) {
			arr[i] *= c;
		}
	}
};

class Node {
public:
	NodeType type;       // helper
	float startDom;      // input
	float endDom;        // input
	float project_val;   // input
	Poly* coeff;          // input
	int final0_coeff;    // output
	float final0_args;   // output, which is `c` for `do_all` call
	virtual void differentiate() = 0;
	virtual void square() = 0;
	virtual void add_const(float c) = 0;
	virtual void mul_const(float c) = 0;
	virtual int do_all(float c) = 0;
	virtual void eval() = 0;
	virtual void build_tree(int depth, int d, int size, float s, float e) = 0;
};
class CNode : public Node {
public:
	Node* left;
	Node* right;
	void differentiate() override {
		if (type==INNER) {
			left->differentiate();
			right->differentiate();
		} else {
			coeff->differentiate();
		}
	}
	void square() override {
		if (type==INNER) {
			left->square();
			right->square();
		} else {
			coeff->square();
		}
	}
	void add_const(float c) override {
		if (type==INNER) {
			left->add_const(c);
			right->add_const(c);
		} else {
			coeff->add_const(c);
		}
	}
	void mul_const(float c) override {
		if (type==INNER) {
			left->mul_const(c);
			right->mul_const(c);
		} else {
			coeff->mul_const(c);
		}
	}
	// this do_all corresponds to `runExp1` in original Grafter benchmark
	int do_all(float c) override {
		differentiate();
		differentiate();
		square();
		square();
		add_const(1);
		mul_const(1);
		add_const(1);
		mul_const(1);
		add_const(1);
		mul_const(1);
		add_const(1);
		mul_const(1);
		return 0;
	}
	void eval() override {
		if (left != nullptr) {
			left->final0_args = final0_args;
			left->eval();
		}
		if (right != nullptr) {
			right->final0_args = final0_args;
			right->eval();
		}
		bool is_leaf = (left==nullptr) && (right==nullptr);
		if (is_leaf) {
			// FIXME
			final0_coeff = do_all(final0_args);
		} else {
			final0_coeff = -1;
		}
	}
	void build_tree(int depth, int d, int size, float s, float e) override {
		if (type==INNER) {
			// INNER
			startDom = s;
			endDom = e;
			project_val = 0.0;
			if (d==depth-1) {
				left = new CNode();
				left->type = LEAF;
				right = new CNode();
				right->type = LEAF;
			} else if (d<depth-1) {
				left = new CNode();
				left->type = INNER;
				right = new CNode();
				right->type = INNER;
			}
			left->build_tree(depth, d+1, size, s, (s+e)/2);
			right->build_tree(depth, d+1, size, (s+e)/2, e);
		} else {
			// LEAF
			startDom = s;
			endDom = e;
			project_val = 0.0;
			coeff = new Poly();
			coeff->assign_coeff(size);
		}
	}
};

class VirtualRoot {
public:
	virtual void eval() = 0;
};
class CVirtualRoot : public VirtualRoot {
public:
	CNode* root;
	void eval() override {
		if (root != nullptr) {
			root->final0_args = 1;
			root->eval();
		}
	}
	void build_tree(int depth, int d, int size, float s, float e) {
		root = new CNode();
		root->type = INNER;
		root->build_tree(depth, d, size, s, e);
	}
};

int main(int argc, char **argv) {
	int n = atoi(argv[1]);

	auto T = new CVirtualRoot();
	T->build_tree(n, 0, 4, -100000, 100000);

	T->eval();

	return 0;
}





