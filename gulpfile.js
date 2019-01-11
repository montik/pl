'use strict'

const gulp = require('gulp');
const sass = require('gulp-sass');
const pl = require('./backend');

gulp.task('develop:css', () => {
	return gulp.src('src/scss/main.scss')
		.pipe(sass({style: 'expanded'}))
		.pipe(gulp.dest('dist/'));
});

gulp.task('watch', () => {
	gulp.watch('src/scss/*.scss', gulp.parallel('develop:css'));
});

gulp.task('pl', () => {
	return gulp.src('src/scss/*.scss')
		.pipe(pl.generate())
		.pipe(gulp.dest('/tmp/'));
})

gulp.task('default', gulp.parallel('watch', 'develop:css'));
