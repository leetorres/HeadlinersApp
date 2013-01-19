-- Table structure for table pages
CREATE TABLE IF NOT EXISTS `tweets` (
  `id` bigint(11) unsigned NOT NULL AUTO_INCREMENT,
  `created_at` varchar(1024),
  `from_user` varchar(64),
  `fromuser_idstr` varchar(64),
  `fromuser_name` varchar(128),
  `id_str` varchar(128) NOT NULL,	
  `isolanguage_code` varchar(32),
  `profileimage_url` varchar(1024),
  `profileimage_urlhttps` varchar(1024),
  `source` varchar(1024),
  `text` varchar(65535),
  `touser_idstr` varchar(64),
  `geo` varchar(64),

  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=3 ;
