"use strict";(self.webpackChunkmolstar_clj=self.webpackChunkmolstar_clj||[]).push([[806],{77806:(e,t,s)=>{s.r(t),s.d(t,{MODULE_NAME:()=>i.p,MODULE_VERSION:()=>i.e,MolViewer:()=>r.y,MolViewerFactory:()=>r.f,ViewerModel:()=>n.ViewerModel,ViewerView:()=>n.ViewerView});var i=s(94234),n=s(72804),r=s(59597)},59597:(e,t,s)=>{s.d(t,{f:()=>a,y:()=>l});var i=s(52607),n=s(39994),r=s(67262),o=s(35256);const d=s(383);class l extends o.Widget{constructor(e){super(),this.rendered=!1,this._ready=new r.PromiseDelegate,this.context=e,this.node.tabIndex=0,this.addClass("jp-MolViewer"),this.stage_obj=new d.Stage(this.node,{backgroundColor:"white"}),document.body.appendChild(this.stage_obj.tooltip),e.ready.then((()=>{this.isDisposed||this._render()}))}[i.Printing.symbol](){return()=>i.Printing.printWidget(this)}get ready(){return this._ready.promise}dispose(){super.dispose()}onUpdateRequest(e){!this.isDisposed&&this.context.isReady&&this._render()}onActivateRequest(e){this.node.focus()}onResize(e){this.stage_obj.setSize(Math.floor(e.width)+"px",Math.floor(e.height)+"px")}addElement(e){Object.assign(e.style,{position:"absolute",zIndex:10}),this.stage_obj.viewer.container.appendChild(e)}createElement(e,t,s){var i=document.createElement(e);return Object.assign(i,t),Object.assign(i.style,s),i}updateInfo(e,t){if(e.setSelection("/"+t),this.info.innerHTML='<dt style="font-weight: bold;">Title</dt><dd>'+e.structure.title+'</dd><dt style="font-weight: bold;">Index</dt><dd>'+t+"</dd>",e.structure.extraData.sdf&&e.structure.extraData.sdf[t]){var s=e.structure.extraData.sdf[t];for(const e in s)this.info.innerHTML+='<dt style="font-weight: bold;">'+e+"</dt><dd><div>"+s[e].join("</div><div>")+"</div></dd>"}}_render(){this.rendered||(this.rendered=!0,this.context.urlResolver.getDownloadUrl(this.context.path).then((e=>{this.stage_obj.loadFile(e).then((e=>{if(e.structure.modelStore.count>1){var t=this.createElement("input",{type:"range",value:0,min:0,max:e.structure.modelStore.count-1,step:1},{top:"12px",left:"12px"});this.info=this.createElement("dl",{},{top:"36px",left:"12px"}),t.oninput=t=>{this.updateInfo(e,t.target.value)},this.addElement(t),this.addElement(this.info),this.updateInfo(e,0)}this.context.path.match(/\.(mol2|sdf?)$/)?e.addRepresentation("ball+stick"):e.addRepresentation("ribbon",{colorScheme:"residueindex"}),e.autoView()}))})))}}class a extends n.ABCWidgetFactory{createNewWidget(e){const t=new l(e);return new n.DocumentWidget({content:t,context:e})}}},94234:(e,t,s)=>{s.d(t,{e:()=>n,p:()=>r});const i=s(8330),n=i.version,r=i.name},8330:e=>{e.exports=JSON.parse('{"name":"molstar-clj","version":"0.1.0","description":"A Mol* Widget for Common Lisp Jupyter","keywords":["molstar","jupyter","jupyterlab","jupyterlab-extension","widgets"],"files":["{dist}/**/*.{js,ts,map}","css/*.css","LICENSE.md"],"homepage":"https://github.com/cando-developers/molstar-clj","bugs":{"url":"https://github.com/cando-developers/molstar-clj/issues"},"license":"MIT","author":{"name":"Tarn W. Burton","email":"twburton@gmail.com"},"main":"dist/index.js","types":"./dist/index.d.ts","repository":{"type":"git","url":"https://github.com/cando-developers/molstar-clj"},"scripts":{"build":"sass --no-source-map node_modules/molstar/lib/mol-plugin-ui/skin/light.scss:css/light.css node_modules/molstar/lib/mol-plugin-ui/skin/blue.scss:css/blue.css node_modules/molstar/lib/mol-plugin-ui/skin/dark.scss:css/dark.css && tsc && jupyter-labextension build","lint":"eslint . --ext .ts --fix","lint-check":"eslint . --ext .ts","prepack":"yarn run build"},"dependencies":{"@jupyter-widgets/base":"^6.0.4","case":"^1.6.3","fp-ts":"^2.16.9","fs":"^0.0.1-security","molstar":"4.5.0","ngl":"2.3.1"},"devDependencies":{"@jupyterlab/application":"^4.0.1","@jupyterlab/builder":"^4.0.1","@types/node":"^20.2.5","@typescript-eslint/eslint-plugin":"^6.3.0","@typescript-eslint/parser":"^5.27.0","eslint":"^8.16.0","eslint-config-standard":"^17.0.0","eslint-plugin-import":"^2.22.0","eslint-plugin-node":"^11.1.0","eslint-plugin-prettier":"^5.0.0","eslint-plugin-promise":"^6.0.0","eslint-plugin-standard":"^5.0.0","lint-staged":"^13.0.3","sass":"^1.77.8","typescript":"^5.1.3"},"jupyterlab":{"extension":"dist/plugin","outputDir":"prebuilt","sharedPackages":{"@jupyter-widgets/base":{"bundled":false,"singleton":true}}},"lint-staged":{"*.ts":["eslint . --ext .ts --fix"]},"prettier":{"singleQuote":true}}')}}]);