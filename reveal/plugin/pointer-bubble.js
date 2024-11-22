// var bubbleIframeMouseMove = function( iframe ){

//     iframe.contentWindow.addEventListener('mousemove', function( event ) {
//         var boundingClientRect = iframe.getBoundingClientRect();

//         var evt = new CustomEvent( 'mousemove', {bubbles: true, cancelable: false})
//         evt.clientX = event.clientX + boundingClientRect.left;
//         evt.clientY = event.clientY + boundingClientRect.top;

//         iframe.dispatchEvent( evt );

//     });

// };
function bubbleIframeMouseMove(iframe){
    // Save any previous onmousemove handler
    var existingOnMouseMove = iframe.contentWindow.onmousemove;
    // Attach a new onmousemove listener
    iframe.contentWindow.onmousemove = function(e){
        // Fire any existing onmousemove listener 
        if(existingOnMouseMove) existingOnMouseMove(e);

        // Create a new event for the this window
        var evt = document.createEvent("MouseEvents");

        // We'll need this to offset the mouse move appropriately
        var boundingClientRect = iframe.getBoundingClientRect();

        // Initialize the event, copying exiting event values
        // for the most part
        evt.initMouseEvent( 
            "mousemove", 
            true, // bubbles
            false, // not cancelable 
            window,
            e.detail,
            e.screenX,
            e.screenY, 
            e.clientX*1.55 + boundingClientRect.left, 
            e.clientY*1.5 + boundingClientRect.top, 
            e.ctrlKey, 
            e.altKey,
            e.shiftKey, 
            e.metaKey,
            e.button, 
            null // no related element
        );

        // Dispatch the mousemove event on the iframe element
        iframe.dispatchEvent(evt);
    };
}

function makeSlideBubbleMouse() {
      // get all iframe in foreground and background of slide
    // and convert NodeList to array
    const backgroundSlide = Reveal.getSlideBackground(Reveal.getCurrentSlide());
    const iframeSlide = Array.prototype.slice.call(Reveal.getCurrentSlide().querySelectorAll('iframe'));
    const iframeBackground = Array.prototype.slice.call(backgroundSlide.querySelectorAll('iframe'));
    
    // filter out non "iframe-visualization" iframes
    let allIframes = [].concat(...[iframeSlide, iframeBackground]);
    allIframes.forEach(function(iframe) {
	bubbleIframeMouseMove(iframe);
	// iframe.contentDocument.body.style.cursor = document.body.classList.contains("no-cursor")?"none":"";
    });
    
}

// // Get the iframe element we want to track mouse movements on
// var selection = document.getElementsByTagName('iframe');
// var iframes = Array.prototype.slice.call(selection);
