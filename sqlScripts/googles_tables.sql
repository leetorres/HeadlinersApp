-- Table structure for table pages
CREATE TABLE IF NOT EXISTS `googles` (
  `id` bigint(11) unsigned NOT NULL AUTO_INCREMENT,
  `title` varchar(1024),
  	  `link` varchar(1024),
  	  `snippet` varchar(1024),
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=3 ;