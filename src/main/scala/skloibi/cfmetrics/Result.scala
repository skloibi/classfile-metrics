package skloibi.cfmetrics

trait Result {
  self =>
  private[this] var _metrics: Map[String, Metric] = Map.empty

  protected def use(metric: Metric): Metric = {
    _metrics = _metrics + (metric.code -> metric)
    metric
  }

  def metrics: Map[String, Metric] = _metrics
}
