import Graph from "graphology";
import Sigma from "sigma";
import { NodeBorderProgram } from "@sigma/node-border";

const container = document.getElementById("sigma-container");
const infoBox = document.getElementById("node-info");
const sliderPointsContainer = document.getElementById("slider-points");
const windowDisplay = document.getElementById("window-display");
const toggleBtn = document.getElementById("toggle-labels");

const graph = new Graph();
const state = { selectedNode: null, selectedNeighbors: null };
let labelsVisible = true; // track label visibility

// -------------------------
// Field → color mapping
const fieldColorsResp = await fetch("public/field_color.json");
const fieldColorMapArray = await fieldColorsResp.json();
const fieldColorMap = {};
fieldColorMapArray.forEach(d => fieldColorMap[d.field] = d.color);

function renderGlobalLegend() {
  const legendContainer = document.getElementById("legend-items");
  legendContainer.innerHTML = "";
  Object.entries(fieldColorMap).forEach(([field, color])=>{
    const item = document.createElement("div");
    item.style.display="flex"; item.style.alignItems="center"; item.style.marginBottom="6px";

    const colorBox = document.createElement("span");
    colorBox.style.display="inline-block";
    colorBox.style.width="20px"; colorBox.style.height="20px";
    colorBox.style.backgroundColor=color; colorBox.style.marginRight="10px";
    colorBox.style.border="1px solid #000"; colorBox.style.flexShrink="0";
    item.appendChild(colorBox);

    const lbl = document.createElement("span");
    lbl.textContent=field;
    item.appendChild(lbl);
    legendContainer.appendChild(item);
  });
}

// -------------------------
// Sigma setup
const renderer = new Sigma(graph, container, {
  renderLabels: true,
  nodeProgramClasses: { border: NodeBorderProgram }
});

renderer.setSetting("nodeReducer", (node, data)=>{
  if(state.selectedNeighbors && !state.selectedNeighbors.has(node)) return { ...data, label:"", color:"#f6f6f6"};
  return data;
});

renderer.setSetting("edgeReducer", (edge, data)=>{
  if(state.selectedNeighbors && state.selectedNode && !graph.hasExtremity(edge, state.selectedNode)) return { ...data, hidden:true};
  return data;
});

// Node click
renderer.on("clickNode", ({ node })=>{
  if(state.selectedNode===node){
    state.selectedNode=null;
    state.selectedNeighbors=null;
    infoBox.style.display="none";
  } else {
    state.selectedNode=node;
    state.selectedNeighbors=new Set(graph.neighbors(node));
    state.selectedNeighbors.add(node);
    const n = graph.getNodeAttributes(node);
    infoBox.innerHTML=`
<div><b>Node Information</b></div>
<div class="field"><span class="label">Title:</span><span class="value">${n.fullTitle ?? ""}</span></div>
<div class="field"><span class="label">Authors:</span><span class="value">${n.authors ?? ""}</span></div>
<div class="field"><span class="label">Date:</span><span class="value">${n.date ?? ""}</span></div>
<div class="field"><span class="label">Citations:</span><span class="value">${n.citations ?? ""}</span></div>
<div class="field"><span class="label">Cluster:</span><span class="value">${n.cluster ?? ""}</span></div>
<div class="field"><span class="label">Domain:</span><span class="value">${n.domain ?? ""}</span></div>
<div class="field"><span class="label">Field:</span><span class="value">${n.field ?? ""}</span></div>`;
    infoBox.style.display="block";
  }
  renderer.refresh();
});

// Stage click
renderer.on("clickStage", ()=>{
  state.selectedNode=null;
  state.selectedNeighbors=null;
  infoBox.style.display="none";
  renderer.refresh();
});

// -------------------------
// Load window
async function loadWindow(windowIndex){
const base = '/network_fields_aggregate'; // your repo name

const nodes = await fetch(`${base}/windows_field/nodes_window_${String(windowIndex+1).padStart(3,"0")}.json`)
  .then(r => r.json());

const edges = await fetch(`${base}/windows_field/edges_window_${String(windowIndex+1).padStart(3,"0")}.json`)
  .then(r => r.json());


  graph.clear();

  nodes.forEach(n=>{
    graph.addNode(n.id,{
      ...n,
      x: typeof n.x==="number" ? n.x : Math.random(),
      y: typeof n.y==="number" ? n.y : Math.random(),
      size: n.size,
      color: n.color ?? "#666",
      type:"border",
      borderColor:"#000",
      label: labelsVisible ? (n.label?.length>30 ? n.label.slice(0,30)+"…" : n.label) : "",
      fullTitle: n.label,
      opacity:1
    });
  });

  edges.forEach(e=>{
    const color = graph.getNodeAttribute(e.source,"color") ?? "#999";
    graph.addEdge(e.source,e.target,{...e,color,opacity:1});
  });

  windowDisplay.textContent = nodes.length>0 && nodes[0].window ? nodes[0].window : `Window ${windowIndex+1}`;
  highlightSliderPoint(windowIndex);
  renderer.refresh();
}

// -------------------------
// Discrete slider points
const totalWindows = 50;

function renderSliderPoints() {
  sliderPointsContainer.innerHTML = "";
  for (let i=0; i<totalWindows; i++){
    const point = document.createElement("div");
    point.className = "slider-point";
    point.style.left = (i/(totalWindows-1))*100 + "%";

    // Tick labels every 5 points
    if(i%5===0){
      const tickLabel = document.createElement("div");
      tickLabel.className="tick-label";
      tickLabel.textContent = `${1970+i}-${1970+i+4}`;
      point.appendChild(tickLabel);
    }

    point.addEventListener("click", ()=> loadWindow(i));
    sliderPointsContainer.appendChild(point);
  }
}

function highlightSliderPoint(idx){
  const points = sliderPointsContainer.children;
  for (let i=0; i<points.length; i++){
    points[i].classList.toggle("active", i===idx);
  }
}

// -------------------------
// Toggle Labels Button
toggleBtn.addEventListener("click", () => {
  labelsVisible = !labelsVisible;
  renderer.refresh(); // redraw nodes with or without labels
  // Force reload of current window to update labels
  const activeIndex = Array.from(sliderPointsContainer.children).findIndex(p => p.classList.contains("active"));
  if(activeIndex >= 0) loadWindow(activeIndex);
});

// -------------------------
// Initialize
renderGlobalLegend();
renderSliderPoints();
loadWindow(0);

