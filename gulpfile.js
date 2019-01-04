'use strict'

const gulp = require('gulp');
const sass = require('gulp-sass');

gulp.task('develop:css', () => {
	return gulp.src('src/scss/main.scss')
		.pipe(sass({style: 'expanded'}))
		.pipe(gulp.dest('dist/'));
});

gulp.task('watch', () => {
	gulp.watch('src/scss/*.scss', gulp.parallel('develop:css'));
});

gulp.task('default', gulp.parallel('watch', 'develop:css'));
