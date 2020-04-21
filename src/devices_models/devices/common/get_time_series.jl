function get_time_series(
    psi_container::PSIContainer,
    device::PSY.Device,
    forecast_label::String,
)
    initial_time = model_initial_time(psi_container)
    @debug initial_time
    use_forecast_data = model_uses_forecasts(psi_container)
    time_steps = model_time_steps(psi_container)

    if use_forecast_data && PSY.has_forecast(device)
        forecast = PSY.get_forecast(
            PSY.Deterministic,
            device,
            initial_time,
            forecast_label,
            length(time_steps),
        )
        return ts_vector = TS.values(PSY.get_data(forecast))
    else
        @debug "device $(PSY.get_name(device)) $(PSY.has_forecast(device)) forecast"
        return ts_vector = ones(time_steps[end])
    end
end
