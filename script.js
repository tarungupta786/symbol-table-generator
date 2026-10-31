const TYPES=new Set(["int","float","double","char","void","long","short","bool"]);
const QUAL=new Set(["const","unsigned","signed","static","extern","register","volatile"]);
const KW=new Set(["if","else","while","for","do","return","switch","case","default","break","continue","sizeof","struct","typedef","goto","true","false"]);
const LIB=new Set(["printf","scanf","malloc","free","strlen","strcpy","strcmp","puts","gets","getchar","putchar","exit","abs","sqrt","pow","main"]);
const SIZES={int:4,float:4,double:8,char:1,void:0,long:8,short:2,bool:1};
const RE=/(?<comment>\/\/[^\n]*|\/\*[\s\S]*?\*\/)|(?<pre>^[ \t]*#[^\n]*)|(?<string>"(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*')|(?<num>\d+\.?\d*(?:[eE][+-]?\d+)?)|(?<id>[A-Za-z_]\w*)|(?<op>==|!=|<=|>=|&&|\|\||\+\+|--|\+=|-=|\*=|\/=|->|<<|>>|[{}()\[\];,=*&+\-\/<>!%.:?|^~])|(?<nl>\n)|(?<ws>[ \t\r]+)|(?<bad>[\s\S])/gm;

function tokenize(src){
  const out=[];let line=1,m;RE.lastIndex=0;
  while((m=RE.exec(src))){
    const g=m.groups,k=Object.keys(g).find(x=>g[x]!==undefined),v=m[0];
    if(k==="nl")line++;
    else if(k==="comment")line+=(v.match(/\n/g)||[]).length;
    else if(k==="ws"||k==="pre"){}
    else if(k==="bad")out.errors=(out.errors||[]).concat(`Line ${line}: illegal character '${v}'`);
    else out.push({kind:k,val:v,line});
  }
  return out;
}

function analyze(src){
  const t=tokenize(src),errors=[...(t.errors||[])],all=[];
  const stack=[{name:"global",tab:new Map()}];let blocks=0,pName=null,pParams=[];
  const lookup=n=>{for(let i=stack.length-1;i>=0;i--)if(stack[i].tab.has(n))return stack[i].tab.get(n);return null};
  const declare=o=>{
    const sc=stack[stack.length-1];const s={size:0,init:"",extra:"",...o,scope:sc.name,level:stack.length-1};
    if(sc.tab.has(s.name)){const p=sc.tab.get(s.name);
      if(p.category==="function"&&s.category==="function")return;
      errors.push(`Line ${s.line}: redeclaration of '${s.name}' in scope '${sc.name}' (first declared at line ${p.line})`);return}
    sc.tab.set(s.name,s);all.push(s)};
  const pk=i=>t[i]||{kind:"eof",val:"",line:0};
  const isT=i=>pk(i).kind==="id"&&(TYPES.has(pk(i).val)||QUAL.has(pk(i).val));
  const bsize=ty=>{for(const w of ty.split(" "))if(w in SIZES)return SIZES[w];return 4};

  function params(i){
    i++;const ps=[];let cur=[],d=0;
    while(pk(i).kind!=="eof"){const v=pk(i).val;
      if(v==="(")d++;
      if(v===")"){if(d===0)break;d--}
      if(v===","&&d===0){ps.push(cur);cur=[]}else cur.push(pk(i));i++}
    if(cur.length)ps.push(cur);
    const out=[];
    for(const p of ps){
      const w=p.filter(x=>TYPES.has(x.val)||QUAL.has(x.val)).map(x=>x.val);
      const st=p.filter(x=>x.val==="*").length;
      const ids=p.filter(x=>x.kind==="id"&&!TYPES.has(x.val)&&!QUAL.has(x.val));
      if(!ids.length)continue;
      const nm=ids[ids.length-1],arr=p.some(x=>x.val==="[");
      out.push({name:nm.val,category:"parameter",line:nm.line,
        type:w.join(" ")+(st?" "+"*".repeat(st):"")+(arr?"[]":""),size:st||arr?8:bsize(w.join(" "))});
    }
    return [out,i+1];
  }

  function decl(i){
    let words=[];while(isT(i)){words.push(pk(i).val);i++}
    const base=words.join(" ");
    for(;;){
      let ptr=0;while(pk(i).val==="*"){ptr++;i++}
      const nm=pk(i);if(nm.kind!=="id")return i-1;i++;
      const type=ptr?base+" "+"*".repeat(ptr):base;
      if(pk(i).val==="("){
        let ps;[ps,i]=params(i);
        declare({name:nm.val,category:"function",type,line:nm.line,extra:"("+ps.map(p=>(p.type+" "+p.name)).join(", ")+")"});
        if(pk(i).val==="{"){pName=nm.val;pParams=ps;return i-1}
        return i;
      }
      const dims=[];
      while(pk(i).val==="["){i++;let d="";while(pk(i).val!=="]"&&pk(i).kind!=="eof"){d+=pk(i).val;i++}dims.push(d);i++}
      let init="";
      if(pk(i).val==="="){i++;let dp=0;const parts=[];
        while(pk(i).kind!=="eof"){const v=pk(i).val;
          if("({[".includes(v))dp++;else if(")}]".includes(v))dp--;
          if(dp===0&&(v===","||v===";"))break;parts.push(v);i++}
        init=parts.join(" ")}
      let size=ptr?8:bsize(base);for(const d of dims)size*=/^\d+$/.test(d)?+d:1;
      declare({name:nm.val,category:dims.length?"array":"variable",type,line:nm.line,size,init,extra:dims.map(d=>"["+d+"]").join("")});
      if(pk(i).val===","){i++;continue}
      return i;
    }
  }

  for(let i=0;i<t.length;i++){
    const k=t[i];
    if(isT(i))i=decl(i);
    else if(k.val==="{"){stack.push({name:pName||"block"+(++blocks),tab:new Map()});pParams.forEach(declare);pName=null;pParams=[]}
    else if(k.val==="}"){if(stack.length>1)stack.pop()}
    else if(k.kind==="id"&&!KW.has(k.val)){
      const prev=i?t[i-1].val:"";
      if(![".","->","struct"].includes(prev)&&!lookup(k.val)&&!LIB.has(k.val))errors.push(`Line ${k.line}: undeclared identifier '${k.val}'`);
    }
  }
  return {symbols:all,errors};
}

/* ---------- UI ---------- */
const DEMO=`#include <stdio.h>
int count = 0;
float PI = 3.14;
int add(int a, int b);

int add(int a, int b) {
    int sum = a + b;      // local
    return sum;
}

int main() {
    int x = 5, y, arr[10];
    char *name = "hello";
    for (int i = 0; i < 10; i++) {
        float temp = i * PI;
        x = x + temp;
    }
    int x;                // redeclaration
    z = add(x, y);        // undeclared
    return 0;
}
`;
const $=id=>document.getElementById(id);
const src=$("src"),view=$("view"),stats=$("stats"),errsEl=$("errs");
let state={symbols:[],errors:[]},mode="table",timer;
const COLS=[["Name","name"],["Category","category"],["Type","type"],["Size","size"],["Scope","scope"],["Lvl","level"],["Line","line"],["Init","init"],["Info","extra"]];
const esc=s=>String(s).replace(/[&<>"]/g,c=>({"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;"}[c]));

function jump(line){
  const lines=src.value.split("\n");let start=0;
  for(let i=0;i<line-1;i++)start+=lines[i].length+1;
  src.focus();src.setSelectionRange(start,start+(lines[line-1]||"").length);
  const lh=parseFloat(getComputedStyle(src).lineHeight)||20;src.scrollTop=Math.max(0,(line-4)*lh);
}
function filtered(){
  const q=$("q").value.toLowerCase(),c=$("cat").value;
  return state.symbols.filter(s=>(!c||s.category===c)&&(!q||[s.name,s.type,s.scope].some(x=>String(x).toLowerCase().includes(q))));
}
function toCSV(rows){return [COLS.map(c=>c[0]).join(",")].concat(rows.map(s=>COLS.map(c=>`"${String(s[c[1]]).replace(/"/g,'""')}"`).join(","))).join("\n")}
function render(){
  const s=state.symbols,sc=new Set(s.map(x=>x.scope)).size,fn=s.filter(x=>x.category==="function").length;
  stats.innerHTML=[["Symbols",s.length],["Scopes",sc],["Functions",fn],["Errors",state.errors.length]].map(([a,b])=>`<div class="stat"><b>${b}</b><span>${a}</span></div>`).join("");
  $("tools").style.display=mode==="table"?"flex":"none";
  const rows=filtered();
  if(mode==="json")view.innerHTML=`<pre id="raw" class="mono">${esc(JSON.stringify(rows,null,2))}</pre>`;
  else if(mode==="csv")view.innerHTML=`<pre id="raw" class="mono">${esc(toCSV(rows))}</pre>`;
  else if(!rows.length)view.innerHTML=`<div class="empty">${s.length?"No symbols match your filter.":"No symbols yet. Declare a variable or function to see it here."}</div>`;
  else view.innerHTML=`<table><thead><tr>${COLS.map(c=>`<th>${c[0]}</th>`).join("")}</tr></thead><tbody>${rows.map(r=>
    `<tr data-line="${r.line}"><td class="mono"><b>${esc(r.name)}</b></td><td><span class="badge" style="color:var(--c-${r.category})">${r.category}</span></td>
     <td class="mono">${esc(r.type)}</td><td>${r.size}</td><td><span class="scope" style="margin-left:${r.level*8}px">${esc(r.scope)}</span></td>
     <td>${r.level}</td><td>${r.line}</td><td class="mono">${esc(r.init)}</td><td class="mono">${esc(r.extra)}</td></tr>`).join("")}</tbody></table>`;
  if(state.errors.length){errsEl.className="errs";errsEl.innerHTML=`<h3>${state.errors.length} problem${state.errors.length>1?"s":""} found</h3>`+
    state.errors.map(e=>`<button class="err" data-line="${(e.match(/^Line (\d+)/)||[])[1]||1}">${esc(e)}</button>`).join("")}
  else{errsEl.className="errs ok";errsEl.textContent=s.length?"✓ No errors found.":""}
}
function run(){try{state=analyze(src.value)}catch(e){state={symbols:[],errors:["Parser error: "+e.message]}}render()}

src.addEventListener("input",()=>{clearTimeout(timer);timer=setTimeout(run,150)});
src.addEventListener("keydown",e=>{if(e.key==="Tab"){e.preventDefault();document.execCommand("insertText",false,"    ")}});
$("q").addEventListener("input",render);$("cat").addEventListener("change",render);
$("demo").onclick=()=>{src.value=DEMO;run()};
$("clear").onclick=()=>{src.value="";run();src.focus()};
document.querySelectorAll("[data-v]").forEach(b=>b.onclick=()=>{
  mode=b.dataset.v;document.querySelectorAll("[data-v]").forEach(x=>x.classList.toggle("on",x===b));render()});
$("copy").onclick=async()=>{
  const rows=filtered(),txt=mode==="csv"?toCSV(rows):JSON.stringify(rows,null,2);
  try{await navigator.clipboard.writeText(txt);$("copy").textContent="Copied ✓"}
  catch(e){mode=mode==="csv"?"csv":"json";render();$("copy").textContent="Select & copy"}
  setTimeout(()=>$("copy").textContent="Copy",1500)};
document.addEventListener("click",e=>{const el=e.target.closest("[data-line]");if(el)jump(+el.dataset.line)});
src.value=DEMO;run();
