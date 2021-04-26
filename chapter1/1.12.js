// SICP JS Ex1.12
// Pascal's Triangle

/* first implementation */
function pascal_triangle(row) {
  function calc(row, idx) {
    return idx === 0 || idx === row - 1
      ? 1
      : calc(row-1, idx-1) + calc(row-1, idx) 
  }

  let res = Array(row);
  for (let i = 0; i < row; i++)
    res[i] = calc(row, i);
  return res;
}

/* final implementation */
function PascalTriangle() {
  return ({
    /* return element i of row r */
    calc : function(row, idx) {
      return idx === 0 || idx === row - 1
        ? 1
        : this.calc(row-1, idx-1) + this.calc(row-1, idx) 
    },
    /* return row n */
    get_row : function(n) {
      let row = Array(n);
      for (let i = 0; i < n; i++)
        row[i] = this.calc(n, i);
      return row;
    },
    /* return rows up to n */
    upto_row : function(n) {
      let triangle = Array(n);
      for (let i = 1; i <= n; i++)
        triangle[i - 1] = this.get_row(i);
      return triangle;
    },
    /* format and print rows up to n */
    pretty_print : function(n) {
      let res = this.upto_row(n).join(",\n");
      console.log(res);
    }
  });
};

const pt = PascalTriangle(); 
console.log(pt.get_row(4));
console.log(pt.upto_row(4));
pt.pretty_print(6);
// console.log(pascal_triangle(4));
