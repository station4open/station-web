'use strict';

(function (run) {
	if (document.readyState === 'loading')
		return document.addEventListener('DOMContentLoaded', run);
	else
		return run();
}(function () {
	var forms = document.getElementsByTagName('form');
	for (var i=0; i<forms.length; ++i)
		(function (form) {
			var checkbox = form.querySelector('input[type="checkbox"][name="delete"]');
			var textboxes = form.querySelectorAll('input[type="text"], input[type="number"], textarea');
			function update() {
				for (var j=0; j<textboxes.length; ++j)
					if (!textboxes.item(j).readOnly)
						textboxes.item(j).disabled = checkbox.checked;
			}
			if (checkbox !== null) {
				checkbox.addEventListener('input', update);
				return update();
			}
		})(forms.item(i));
}))
