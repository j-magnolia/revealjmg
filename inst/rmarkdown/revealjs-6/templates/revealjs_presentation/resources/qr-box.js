var qrbox = document.querySelector("#qrbox");
var advance_fragment = 0;

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
