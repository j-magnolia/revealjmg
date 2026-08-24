function createSingletonNodejg( container, tagname, classname, innerHTML ) {
  // Find all nodes matching the description
  var nodes = container.querySelectorAll( '.' + classname );

  // Check all matches to find one which is a direct child of
  // the specified container
  for( var i = 0; i < nodes.length; i++ ) {
  	var testNode = nodes[i];
  	if( testNode.parentNode === container ) {
  		return testNode;
  	}
  }

  // If no node was found, create it now
  var node = document.createElement( tagname );
  node.className = classname;
  if( typeof innerHTML === 'string' ) {
  	node.innerHTML = innerHTML;
  }
  container.appendChild( node );

  return node;
}

var dom_wrapper = document.querySelector('.reveal');

createSingletonNodejg(dom_wrapper, 'div', 'qrbox',
  '<div class="qrbox" id="qrbox" style="font-size:90%;">' + '\n' +
  '<div style="font-size:30%;width:100%;">' + '\n' +
  $if(pageurl)$
    '<a href="https://$pageurl$">' +
  	'<img src="$qrimage$" alt="https://$pageurl$"/>' +
  	'</a>' + '\n' +
	$else$
	  '<img src="$qrimage$"/>' + '\n' +
	$endif$
  '</div>' + '\n' +
  '<div style="font-size:30%;width:100%;vertical-align:top;">' + '\n' +
    '<span style="display:inline-block;text-align:left;margin-left:0">' + '\n' +
  $if(pageurl)$
      'Live web page: <a href="https://$pageurl$">https://$pageurl$</a>' + '\n' +
      $if(pdfurl)$
        '<br/>' + '\n' +
        'PDF: <a href="https://$pdfurl$" target="_blank">https://$pdfurl$</a>' + '\n' +
      $endif$
  $endif$
  	'</span>' + '\n' +
  	'<span style="display:inline-block;text-align:right;vertical-align:top;position:absolute;right:0;bottom:0;">' + '\n' +
  	  'Navigate slides: next: N or &lt;space&gt;; previous: P or &lt;backspace&gt;<br/>' + '\n' +
  	  'Also: up, down, left, right arrows; overview: o; help: ?' + '\n' +
  	'</span>' + '\n' +
	'</div>' + '\n' +
  '</div>' + '\n'
  );

var qrbox = document.querySelector("#qrbox");
var advance_fragment = 0;

if (qrbox != null) {
  function isPrintingPDF() {
    let printing = ( /print-pdf/gi ).test( window.location.search );
    console.log("printing test: " + printing);
    return printing;
  }

  if ( qrbox.hasAttribute('qr-box-hide') || Reveal.isOverview() ||
      ! Reveal.isFirstSlide() || isPrintingPDF()) {
    console.log("Initializing");
    console.log("Hiding QR box");
    qrbox.style.visibility="hidden";
    qrbox.style.display="none";
  }

  Reveal.addEventListener('overviewshown', function() {
    console.log("Overview shown");
    console.log("Hiding QR box");
    qrbox.style.visibility="hidden";
    qrbox.style.display="none";
  }, false);

  Reveal.addEventListener('overviewhidden', function() {
    if (Reveal.isFirstSlide() && ! qrbox.hasAttribute('qr-box-hide') &&
        ! isPrintingPDF()) {
      console.log("Overview hidden");
      console.log("Showing QR box");
      qrbox.style.visibility="visible";
      qrbox.style.display="block";
    }
  }, false);

  Reveal.addEventListener('slidechanged', function() {
    console.log("Slide changed...");
    if (Reveal.isFirstSlide() && ! Reveal.isOverview() &&
        ! qrbox.hasAttribute('qr-box-hide') &&
        ! isPrintingPDF()) {
      console.log("Showing QR box");
      qrbox.style.visibility="visible";
      qrbox.style.display="block";
    } else {
      console.log("Hiding QR box");
      qrbox.style.visibility="hidden";
      qrbox.style.display="none";
    }
  }, false);

  Reveal.addEventListener('pdf-ready', function() {
    console.log("hiding qrbox for printing");
    qrbox.style.visibility="hidden";
    qrbox.style.display="none";
    qrbox.setAttribute('qr-box-hide', 'true');
  });
}

Reveal.addEventListener('slidechanged', function() {
  while (advance_fragment > 0) {
    // console.log('advancing fragment');
    Reveal.nextFragment();
    advance_fragment--;
  }
}, false);

Reveal.addEventListener('slidechanged', function() {
  if ( Reveal.getCurrentSlide().hasAttribute('data-skip')) {
    // console.log("going to next slide...");
    Reveal.next();
  }
}, false);

Reveal.addEventListener('skip_slide', function() {
  Reveal.next();
}, false);

Reveal.addEventListener('advance_fragment', function() {
  // console.log("setting advance fragment");
  advance_fragment++;
  }, false);
