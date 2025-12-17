#lang rhombus/scribble/manual
@(import:
    "common.rhm" open:
      except: def
    "nonterminal.rhm" open
    meta_label:
      rhombus/date)

@(def date_eval: make_rhombus_eval())
@examples(
  ~hidden: #true
  ~eval: date_eval
  import rhombus/date
)

@title(~style: #'toc, ~tag: "date"){Date and Time}

@docmodule(rhombus/date)

The @rhombusmodname(rhombus/date) library provides basic facilities for
working with dates and times. A @rhombus(date.Time, ~class) represents
just a time (relative to an unspecified date and time zone),
@rhombus(date.Date, ~class) represents just a day (relative to an
unspecified time zone), a @rhombus(date.DateTime, ~class) has both a
day and time, and a @rhombus(date.ZonedDateTime, ~class) represents an
absolute point in time as a date and time in a specific time zone.

@local_table_of_contents()

@// ============================================================

@section(~tag: "date-time"){Time}

@doc(
  class date.Time(
    ~hour: hour :: Int.in(0 ..= 23) = 0,
    ~minute: minute :: Int.in(0 ..= 59) = 0,
    ~second: second :: Int.in(0 ..= 59) = 0,
    ~nanosecond: nanosecond :: Int.in(0 ..= 999_999_999) = 0
  )
){

 Represents a time relative to an unspecified date and in an unspecified
 time zone.

 The @rhombus(date.Time, ~annot) class privately implements
 @rhombus(Comparable, ~class), so @rhombus(date.Time, ~annot) instances
 can be compared with operators like @rhombus(<) and @rhombus(>).
 A @rhombus(date.Time, ~annot) instance is @tech{serializable}.

@examples(
  ~eval: date_eval
  ~repl:
    def tm = date.Time(~hour: 5, ~minute: 45)
    tm
    tm.to_string()
    tm < date.Time(~hour: 6, ~minute: 10)
)

}

@doc(
  fun date.Time.now(~local: local :: Any = #true) :: date.Time
){

 Uses the system clock via @rhombus(system.milliseconds) to get the
 current time and using the operating system's facilities to get a time
 of day relative to either the current time zone (when @rhombus(local) is
 true) or UTC (when @rhombus(local) is false).

}


@doc(
  fun date.Time.from_seconds(
    secs :: Real,
    ~local: local :: Any = #true
  ) :: date.Time
){

 Uses operating system facilities to convert a number of seconds since
 @tech{the epoch} to a time of day relative to either the current time
 zone (when @rhombus(local) is true) or UTC (when @rhombus(local) is
 false).

 If the operating system reports a time within a leap second, then the
 reported time is the end of the preceding second.

@examples(
  ~eval: date_eval
  ~repl:
    date.Time.from_seconds(0, ~local: #false)
)

}


@doc(
  method (tm :: date.Time).to_string(
    ~time_format: time_format :: date.TimeFormat = #'seconds
  ) :: String
){

 Converts @rhombus(tm) to a string format depending on
 @rhombus(time_format). See @rhombus(date.TimeFormat, ~annot)
 for more information.

@examples(
  ~eval: date_eval
  ~repl:
    def tm1 = date.Time(~hour: 5, ~minute: 45, ~second: 1, ~nanosecond: 70000)
    tm1.to_string()
    tm1.to_string(~time_format: #'nanoseconds)
    tm1.to_string(~time_format: #'auto_subseconds)
)

}

@// ============================================================

@section(~tag: "date-date"){Date}

@doc(
  class date.Date(
    ~year: year :: Int,
    ~month: month :: Int.in(1 ..= 12) = 1,
    ~day: day :: Int.in(1 ..= date.days_in_month(year, month)) = 1
  )
){

 Represents a date relative to an unspecified time zone.

 The @rhombus(date.Date, ~annot) class privately implements
 @rhombus(Comparable, ~class), so @rhombus(date.Date, ~annot) instances
 can be compared with operators like @rhombus(<) and @rhombus(>).
 A @rhombus(date.Date, ~annot) instance is @tech{serializable}.

@examples(
  ~eval: date_eval
  ~repl:
    def dt = date.Date(~year: 1968, ~month: 1, ~day: 24)
    dt
    dt.to_string()
    dt < date.Date(~year: 1969, ~month: 7, ~day: 20)
    ~error:
      date.Date(~year: 1999, ~month: 2, ~day: 29)
)

}


@doc(
  property (dt :: date.Date).week_day :: Int.in(0 ..= 6)
  property (dt :: date.Date).year_day :: Int.in(0 ..= 366)
){

 Reports a day of the week or day of the year represented by
 @rhombus(dt).

@examples(
  ~eval: date_eval
  ~repl:
    date.Date(~year: 1968, ~month: 1, ~day: 24).week_day
)

}


@doc(
  fun date.Date.now(~local: local :: Any = #true) :: date.Date
){

 Uses the system clock via @rhombus(system.milliseconds) to get the
 current time and using the operating system's facilities to get a date
 relative to either the current time zone (when @rhombus(local) is true)
 or UTC (when @rhombus(local) is false).

}


@doc(
  fun date.Date.from_seconds(
    secs :: Real,
    ~local: local :: Any = #true
  ) :: date.Date
){

 Uses operating system facilities to convert a number of seconds since
 @tech{the epoch} to a date relative to either the current time
 zone (when @rhombus(local) is true) or UTC (when @rhombus(local) is
 false).

@examples(
  ~eval: date_eval
  ~repl:
    date.Date.from_seconds(0, ~local: #false)
)

}


@doc(
  method (dt :: date.Date).to_string(
    ~format: format :: date.Format = #'rfc3339
  ) :: String
){

 Converts @rhombus(dt) to a string using the format selected by
 @rhombus(format).

@examples(
  ~eval: date_eval
  ~repl:
    def dt1 = date.Date(~year: 1968, ~month: 1, ~day: 24)
    dt1.to_string()
    dt1.to_string(~format: #'american)
)

}


@doc(
  method (dt :: date.Date).to_datetime(
    ~time: tm :: date.Time = date.Time()
  ) :: date.DateTime
){

 Converts a @rhombus(date.Date) to a @rhombus(date.DateTime) using
 @rhombus(tm) for the time portion of the new @rhombus(date.DateTime),
 and using UTC as the time zone.

}

@doc(
  method (dt :: date.Date).to_seconds() :: Real
){

 Equivalent to @rhombus(dt.to_datetime().to_seconds()).

}

@// ============================================================

@section(~tag: "date-datetime"){Date and Time Combined}

@doc(
  class date.DateTime(
    ~year: year :: Int,
    ~month: month :: Int.in(1 ..= 12) = 1,
    ~day: day :: Int.in(1 .. date.days_in_month(year, month)) = 1,
    ~hour: hour :: Int.in(0 ..= 23) = 0,
    ~minute: minute :: Int.in(0 ..= 59) = 0,
    ~second: second :: Int.in(0 ..= 59) = 0,
    ~nanosecond: nanosecond :: Int.in(0 ..= 999_999_999) = 0
  )
){

 Combines the fields of @rhombus(date.Date) and @rhombus(date.Time) to
 represent a specific date and time relative to an unspecified time zone.

 The @rhombus(date.DateTime, ~annot) class privately implements
 @rhombus(Comparable, ~class), so @rhombus(date.DateTime, ~annot) instances
 can be compared with operators like @rhombus(<) and @rhombus(>).
 A @rhombus(date.DateTime, ~annot) instance is @tech{serializable}.

@examples(
  ~eval: date_eval
  ~repl:
    def dt = date.DateTime(~year: 2025, ~month: 11, ~day: 14,
                           ~hour: 18, ~minute: 14)
    dt
    dt.to_string()
    dt > date.DateTime.from_seconds(0)
)

}


@doc(
  property (dt :: date.DateTime).week_day :: Int.in(0 ..= 6)
  property (dt :: date.DateTime).year_day :: Int.in(0 ..= 366)
){

 Reports a day of the week or day of the year represented by
 @rhombus(dt).

@examples(
  ~eval: date_eval
  ~repl:
    date.DateTime(~year: 2025, ~month: 11, ~day: 14).week_day
)

}


@doc(
  fun date.DateTime.now(~local: local :: Any = #true) :: date.DateTime
){

 Like @rhombus(date.Date.now), but preserving the current time within
 the current day.

}


@doc(
  fun date.DateTime.from_seconds(
    secs :: Real,
    ~local: local :: Any = #true
  ) :: date.TimeDate
){

 Like @rhombus(date.Date.now), but preserving the time of @rhombus(secs)
 within the day of @rhombus(secs).

 If the operating system reports a time within a leap second, then the
 reported time is the end of the preceding second.

@examples(
  ~eval: date_eval
  ~repl:
    date.DateTime.from_seconds(0, ~local: #false)
)

}


@doc(
  method (dt :: date.DateTime).to_string(
    ~format: format :: date.Format = #'rfc3339,
    ~time_format: time_format :: maybe(date.TimeFormat) = #'seconds,
  ) :: String
){

 Converts @rhombus(dt) to a string using the format selected by
 @rhombus(format). If @rhombus(time_format) is not @rhombus(#false), then
 the string includes the time portion of @rhombus(dt). The use of
 @rhombus(time_format) may be limited by @rhombus(format). If a
 @rhombus(format) requires a time zone, then UTC is used.

@examples(
  ~eval: date_eval
  ~repl:
    def dt1 = date.DateTime(~year: 1968, ~month: 1, ~day: 24, ~second: 10)
    dt1.to_string()
    dt1.to_string(~format: #'rfc2822)
)

}


@doc(
  method (dt :: date.DateTime).to_zoned() :: date.ZonedDateTime
){

 Converts a @rhombus(date.DateTime) to a @rhombus(date.ZonedDateTime)
 using using UTC as the time zone. This conversion can fail if the date
 falls outside the (large) range of dates that the operating system can
 locate.

}

@doc(
  method (dt :: date.DateTime).to_seconds() :: Real
){

 Equivalent to @rhombus(dt.to_zoned().to_seconds()).

}

@// ============================================================

@section(~tag: "date-zoneddatetime"){Date and Time in a Time Zone}

@doc(
  class date.ZonedDateTime(
    ~year: year :: Int,
    ~month: month :: Int.in(1 ..= 12) = 1,
    ~day: day :: Int.in(1 ..= date.days_in_month(year, month)) = 1,
    ~hour: hour :: Int.in(0 ..= 23) = 0,
    ~minute: minute :: Int.in(0 ..= 59) = 0,
    ~second: second :: Int.in(0 ..= 60) = 0,
    ~nanosecond: nanosecond :: Int.in(0 ..= 999_999_999) = 0,
    ~is_dst: is_dst :: Boolean = #false,
    ~time_zone_offset: time_zone_offset :: Int = 0,
    ~time_zone_name: time_zone_name :: String = "UTC"
  )
){

 Represents an absolute date and time within a specific time zone. The
 @rhombus(time_zone_offset) argument is in seconds. There is no checking
 that @rhombus(time_zone_offset), @rhombus(is_dst), and and
 @rhombus(time_zone_name) are consistent, but they will be filled in
 correctly when a function like @rhombus(date.ZonedDateTime.now)
 @rhombus(date.ZonedDateTime.from_seconds) is used to create a
 @rhombus(date.ZonedDateTime, ~class).

 Unlike @rhombus(date.DateTime, ~class),
 @rhombus(date.ZonedDateTime, ~annot) accommodates a leap second. There
 is no checking when a @rhombus(date.ZonedDateTime, ~annot) is created
 that the leap second is appropriate to the date.

 The @rhombus(date.ZonedDateTime, ~annot) class privately implements
 @rhombus(Comparable, ~class), so @rhombus(date.ZonedDateTime, ~annot)
 instances can be compared with operators like @rhombus(<) and
 @rhombus(>). Comparison involves conversion via
 @rhombus(date.ZonedDateTime.to_seconds).
 A @rhombus(date.ZonedDateTime, ~annot) instance is @tech{serializable}.

}


@doc(
  property (dt :: date.ZonedDateTime).week_day :: Int.in(0 ..= 6)
  property (dt :: date.ZonedDateTime).year_day :: Int.in(0 ..= 366)
){

 Reports a day of the week or day of the year represented by
 @rhombus(dt).

}


@doc(
  fun date.ZonedDateTime.now(~local: local :: Any = #true) :: date.DateTime
){

 Like @rhombus(date.DateTime.now), but preserving the time zone selected
 by @rhombus(local) and (if true) the current time zone, and with a leap
 second (if any) preserved.

}


@doc(
  fun date.ZonedDateTime.from_seconds(
    secs :: Real,
    ~local: local :: Any = #true
  ) :: date.TimeDate
){

 Like @rhombus(date.DateTime.from_seconds), but preserving the time zone
 selected by @rhombus(local) and (if true) the current time zone.

@examples(
  ~eval: date_eval
  ~repl:
    date.ZonedDateTime.from_seconds(0, ~local: #false)
)

}


@doc(
  method (dt :: date.ZonedDateTime).to_string(
    ~format: format :: date.Format = #'rfc3339,
    ~time_format: time_format :: maybe(date.TimeFormat) = #'seconds,
    ~time_zone_format: tz_format :: maybe(date.TimeZoneFormat) = #'offset
  ) :: String
){

 Converts @rhombus(dt) to a string in a similar way as
 @rhombus(date.DateTime.to_string), but with an extra option
 @rhombus(tz_format) to determine whether a time zone is shown if
 it is optional for @rhombus(format).

@examples(
  ~eval: date_eval
  ~repl:
    def dt1 = date.ZonedDateTime(~year: 1968, ~month: 1, ~day: 24, ~second: 10,
                                 ~time_zone_offset: 7 * 60 * 60,
                                 ~time_zone_name: "MDT")
    dt1.to_string()
    dt1.to_string(~format: #'rfc2822)
)

}


@doc(
  method (dt :: date.ZonedDateTime).to_seconds() :: Real
){

 Returns the number of seconds since @tech{the epoch} until
 @rhombus(dt).

}

@// ============================================================

@section(~tag: "date-format"){Date Formats and Utilities}

@doc(
  enum date.Format
  | rfc2822
  | rfc3339
  | iso8601
  | american
  | european
  | julian
){

 A format supported by @rhombus(date.Date.to_string),
 @rhombus(date.DateTime.to_string), and
 @rhombus(date.ZonedDateTime.to_string):

@itemlist(

  @item{@rhombus(#'rfc2822): follows
   @hyperlink("https://datatracker.ietf.org/doc/html/rfc2822"){RFC 2822}.}

  @item{@rhombus(#'rfc3339): follows
   @hyperlink("https://datatracker.ietf.org/doc/html/rfc3339"){RFC 3339},
   specifically using a space character between a date and time, and
   showing a time zone offset (if requested) as @litchar{Z},
   @tt{@litchar{+}HH@litchar{:}MM} or @tt{@litchar{+}HH@litchar{:}MM}.}

  @item{@rhombus(#'iso8601): follows ISO 8601, showing a time zone offset
   (if requested) as @litchar{Z}, @tt{@litchar{+}HH@litchar{:}MM}, or
   @tt{@litchar{+}HH@litchar{:}MM}.}

  @item{@rhombus(#'american): shows a date in an English, human-readable
   form that spells out a month and day of the week, putting the month
   before the day.}

  @item{@rhombus(#'european): shows a date in an English, human-readable
   form that spells out a month and day of the week, putting the date
   before the month.}

  @item{@rhombus(#'julian): shows the date in Julian form.}

)

}


@doc(
  enum date.TimeFormat
  | minutes
  | seconds
  | milliseconds
  | microseconds
  | nanoseconds
  | auto_subminutes
  | auto_subseconds
){

 A time format that selects the smallest time granularity to be shown,
 sometimes used as @rhombus(maybe(date.TimeZoneFormat), ~annot) so that
 @rhombus(#false) means that a time is not shown.

@itemlist(

 @item{@rhombus(#'minutes): Produces @tt{HH@litchar{:}MM}}

 @item{@rhombus(#'seconds): Produces @tt{HH@litchar{:}MM@litchar{:}SS}}

 @item{@rhombus(#'milliseconds): Produces @tt{HH@litchar{:}MM@litchar{:}SS@litchar{.}SSS}}

 @item{@rhombus(#'microseconds): Produces @tt{HH@litchar{:}MM@litchar{:}SS@litchar{.}SSSSSS}}

 @item{@rhombus(#'nanoseconds): Produces @tt{HH@litchar{:}MM@litchar{:}SS@litchar{.}SSSSSSSSS}}

 @item{@rhombus(#'auto_subminutes): The same as @rhombus(#'minutes) if
  seconds and nanoseconds are zero, the same as @rhombus(#'auto_subseconds) otherwise.}

 @item{@rhombus(#'auto_subseconds): The same as @rhombus(#'seconds) if
  nanoseconds are zero, and otherwise the same as
  @rhombus(#'milliseconds), @rhombus(#'microseconds), or
  @rhombus(#'nanoseconds) depending on the number of trailing zeros
  available to be omitted.}

)

}


@doc(
  enum date.TimeZoneFormat
  | offset
){

 A time-zone format. Although only one format is currently supported,
 this annotation is used as @rhombus(maybe(date.TimeZoneFormat), ~annot)
 so that @rhombus(#false) means that a time zone is not shown.

}


@doc(
  fun date.is_leap_year(year :: Int) :: Boolean
  fun date.days_in_year(year :: Int) :~ Int.in(365 ..= 366)
){

 Reports whether a year is a leap year or whether it has 365 or 366
 days.

}

@doc(
  fun date.days_in_month(year :: Int,
                         month :: Int.in(1 ..= 12)) :: Int.in(28 ..= 31)
){

 Returns the number of days in a given month during a specific year.

}

@doc(
  fun date.year_day(
    year :: Int,
    month :: Int.in(1 ..= 12),
    day :: Int.in(1 ..= date.days_in_month(year, month))
  ) :: Int.in(0 ..= 365)
){

 Reports the index of a given day for a specific month within a specific
 year.

}

@doc(
  fun date.week_day(
    year :: Int,
    month :: Int.in(1 ..= 12),
    day :: Int.in(1 ..= date.days_in_month(year, month))
  ) :: Int.in(0 ..= 6)
){

 Reports the index of a day of the week for a given day of a specific
 month within a specific year.

}
