//Sdocument.body.style.backgroundColor = "red";

document.title = "EDGAUGMENTOR";

shinyjs.alerta = function(text){
  alert(text);
}
console.log("script.js says 'yo'");

// (function() {
//   var link = document.querySelector("link[rel*='icon']") || document.createElement('link');
//   link.type = 'image/x-icon';
//   link.rel = 'shortcut icon';
//   link.href = 'http://www.oweng.net/EDGAugmentor/favicon.ico';
//   document.getElementsByTagName('head')[0].appendChild(link);
//   console.log("did it do anything?");
// })();

shinyjs.breaker = function(){
  console.log("------------------------------------");
}

// prepend any calls with "await ", e.g. "await sleep(2000);"
// https://www.sitepoint.com/delay-sleep-pause-wait/ - can this even work w/shinyjs?
function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

/**
 * params arg with three required attributes
 * @param {string} data_value - possible data-entity value, e.g. "org" or "date"
 * @param {string} data_type - possible data-* element, e.g. "entity" for data-entity attribute
 * @param {number} num_changes - total number of checkboxes changed (may be > 1 if Select All/None clicked)
 */
 shinyjs.updateHighlight = function(params){
  let frameObj = document.getElementById("tenkFrame");
  if(frameObj === null) {
    console.warn("TODO: iframe not found initially? investigate as appropriate");
  } else {
   // chrome/opera/edge/vivaldi req am-i-loaded id be at very bottom of html, FF/Safari ok either way
  let item_iframe =  frameObj.contentWindow.document.getElementById("item_end");
  if(item_iframe === null) {
    delay_ms = 0.5;
    console.log(`getElementById() = null, retry with setTimeout, delay: ${delay_ms}`)
    setTimeout(
      function() {
              shinyjs.updateHighlight(params);
        }, delay_ms);
    } else {
      // console.log(`item html: ${item_iframe.innerHTML}`)
      // frameObj.addEventListener('load', function(){console.log("loaded")} );
      let selector = `[data-${params.data_type}="${params.data_value}"]`;
      let all_of_type = frameObj.contentWindow.document.querySelectorAll(selector);
      prefix = params.num_changes < 1 ? 'un' : ''
      abs_num_changed = Math.abs(params.num_changes)
      if (params.num_changes != 0) {
        let msg = `Multiple types ${prefix}marked, total: ${abs_num_changed}`
        if (Math.abs(params.num_changes) == 1) {
          msg = `${all_of_type.length} ${params.data_value.toUpperCase()} ${prefix}marked`;
        }
        Shiny.onInputChange("updateHighlightMsg", msg);
        console.log(`${all_of_type.length} items OF type ${params.data_value} to be ${prefix}marked`)
      }
      all_of_type.forEach(function(dtype) {
        if (params.num_changes < 1) {
            dtype.classList.add('no-highlight');
        } else {
          dtype.classList.remove('no-highlight');
        }
      });
    }
  }
  return 55;
}
