document.addEventListener('DOMContentLoaded', function () {
    document.querySelectorAll('.fold > summary').forEach(function (summary) {
        summary.addEventListener('click', function () {
            summary.parentNode.classList.toggle('open');
        });
    });
});
